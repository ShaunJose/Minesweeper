-- @author: Shaun Jose --
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- ThreePennyGUI imports
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- haskell library imports
import System.Random
import Data.IORef
import Control.Monad.Trans (liftIO)

-- source file imports
import CustomDataTypes
import MinesweeperFuncs -- non ui functions of minesweeper
import Configs  -- constants and other configurations

-- Main method
main :: IO ()
main = do
  startGUI defaultConfig uiSetup

-- sets up the UI for the game, and the game itself
uiSetup :: Window -> UI ()
uiSetup window = do
    return window # set title "Minesweeper_AI"

    -- define canvas and style
    canvas <- UI.canvas
      # set UI.height (ceiling canvasHeight)
      # set UI.width (ceiling canvasWidth)
      # set UI.style [("border", "solid black 1px"), ("background", "lightgray")]
      # set UI.textFont "12px sans-serif"
      # set UI.strokeStyle "black"
      # set UI.textAlign UI.Center

    -- define buttons + strings written on them
    human <- UI.button -- play as human button option
      #+ [string "Human player"]
    ai    <- UI.button -- play as AI button option
      #+ [string "AI player"]
    reset <- UI.button -- reset game button
      #+ [string "Reset game"]
    changeMode <- UI.button -- change mode to reveal/flagmode as a human player
      #+ [string revealMsg]
    playButton <- UI.button -- play move button for the AI player
      #+ [string "Play move"]

    -- IOReferences (Variables)
    coord <- liftIO $ newIORef (0,0) -- coordinates of mouse on canvas
    g     <- liftIO $ newStdGen -- uses the split method to create newGen
    board <- liftIO $ newIORef (fillBoard (createBoard rows cols 0 (fst $ chooseMines g rows cols [] mines)) (createBoard rows cols 0 (fst $ chooseMines g rows cols [] mines))) -- the board
    clickMode <- liftIO $ newIORef RevealMode -- you reveal mines by default
    gameStat <- liftIO $ newIORef Ongoing -- current game status
    player <- liftIO $ newIORef AI -- human or AI playing?

    -- add canvas, and play as human/AI buttons
    getBody window #+
      [row [element canvas], element human, element ai]

    -- if chosen to play as a human..
    on UI.click human $ \_ ->
      do
        UI.delete human -- delete human option button
        UI.delete ai -- delete AI option button
        currBoard <- liftIO $ readIORef board -- read board
        canvas # set' UI.fillStyle (UI.htmlColor "darkgray") -- for cells
        createBoardUI (0.0, 0.0) rows cols canvas -- create the board UI
        getBody window #+ [element reset, element changeMode] -- add reset, changeMode buttons
        liftIO $ writeIORef player Human -- set the player type to Human
    -- if chosen to play as AI
    on UI.click ai $ \_ ->
      do
        UI.delete human -- delete human option button
        UI.delete ai -- delete AI option button
        currBoard <- liftIO $ readIORef board -- read board
        canvas # set' UI.fillStyle (UI.htmlColor "darkgray") -- for cells
        createBoardUI (0.0, 0.0) rows cols canvas -- create the board UI
        getBody window #+ [element reset, element playButton] -- add reset and playButton buttons
        liftIO $ writeIORef player AI -- set the player type to AI

    -- changeMode for the Human (from reveal to flag or vice versa)
    on UI.click changeMode $ \_ ->
      do
        mode <- liftIO $ readIORef clickMode -- get current mode
        case mode of
          RevealMode -> -- if it's in revealmode, change to flag mode
            do
              liftIO $ writeIORef clickMode FlagMode
              element changeMode  -- change button text appropriately
                # set UI.text flagMsg
          FlagMode -> -- if it's in flagmode, change to reveal mode
            do
              liftIO $ writeIORef clickMode RevealMode
              element changeMode  -- change button text appropriately
                # set UI.text revealMsg

    -- play move button for the AI
    on UI.click playButton $ \_ ->
      do
        boardVal    <- liftIO $ readIORef board -- read board
        gameStatVal <- liftIO $ readIORef gameStat -- read game status
        (newBoard, newGameStatus) <- playAI boardVal gameStatVal canvas
        liftIO $ writeIORef board newBoard -- write new board
        liftIO $ writeIORef gameStat newGameStatus -- write new game status
    on UI.mousemove canvas $ \(x,y) ->
      do liftIO $ writeIORef coord (x,y) -- save the current mouse coordinates

    -- respond to clicks on canvas only if Human is playing an unfinished game
    on UI.click canvas $ \_ ->
      do
        playerType <- liftIO $ readIORef player -- read player type
        currGameStatus <- liftIO $ readIORef gameStat -- read the game's status
        case (playerType, currGameStatus) of -- only proceed to play if game is ongoing and the player is playing
          (Human, Ongoing) ->
            do
              (x, y)     <- liftIO $ readIORef coord
              mode    <- liftIO $ readIORef clickMode
              case mode of
                RevealMode -> -- revealing the cell
                  do
                    boardVal    <- liftIO $ readIORef board -- read board
                    gameStatVal <- liftIO $ readIORef gameStat -- read game status
                    (newBoard, newGameStatus) <- revealResponse boardVal gameStatVal canvas (fromIntegral x, fromIntegral y) --reveal func
                    liftIO $ writeIORef board newBoard -- write newBoard
                    liftIO $ writeIORef gameStat newGameStatus -- write new game Status
                FlagMode   -> -- un/flagging the cell
                  do
                    boardVal    <- liftIO $ readIORef board -- read board
                    newBoard <- flagResponse boardVal canvas (fromIntegral x, fromIntegral y) -- flag func
                    liftIO $ writeIORef board newBoard -- write newBoard
          otherwise       -> return () -- case AI player or game over

    -- button to reset the game
    on UI.click reset $ \_ -> -- reset = delete everything, start over
      do
        UI.delete canvas -- delete everything
        UI.delete reset
        UI.delete changeMode
        UI.delete playButton
        uiSetup window -- start over from the beginning


-- Board UI Creation --
-- creates the board UI
createBoardUI :: UI.Point -> RowNum -> ColNum -> UI.Canvas -> UI ()
createBoardUI (xPos, yPos) rows cols canvas | rows < 1 =
                                                error "createBoardUI: rows < 1"
createBoardUI (xPos, yPos) 1 cols canvas =
    createRowUI (xPos + xGap, yPos + yGap) cols canvas --create last row
createBoardUI (xPos, yPos) rows cols canvas =
  do
    createRowUI (xPos + xGap, yPos + yGap) cols canvas
    createBoardUI (xPos, yPos + yGap + cellHeight) (rows - 1) cols canvas

-- creates the UI for a row of cells
createRowUI :: UI.Point -> ColNum -> UI.Canvas -> UI ()
createRowUI coord cols canvas | cols < 1 = error "createRowUI: cols < 1"
createRowUI coord 1 canvas = canvas # UI.fillRect coord cellWidth cellHeight --last cell creation (in last column)
createRowUI (xPos, yPos) cols canvas =
  do
    canvas # UI.fillRect (xPos, yPos) cellWidth cellHeight -- cells just rects
    createRowUI (xPos + cellWidth + xGap, yPos) (cols - 1) canvas


-- Making and Handling Moves --
-- responds to click on the canvas by the Human, when in RevealMode
revealResponse :: Board -> GameStatus -> UI.Canvas -> UI.Point -> UI (Board, GameStatus)
revealResponse board gameStat canvas coord =
    let (rowIndex, colIndex) = getClickedCellNum coord
        cellClicked          = findCell board (rowIndex, colIndex)
      in makeMove canvas board cellClicked

-- responds to click on the canvas by the Human, when in FlagMode
flagResponse :: Board -> UI.Canvas -> UI.Point -> UI Board
flagResponse board canvas coord =
    let (rowIndex, colIndex) = getClickedCellNum coord
        cellClicked          = findCell board (rowIndex, colIndex)
      in do
          newBoard <- manageFlagging canvas board cellClicked
          return newBoard

-- make move for AI (called when play button is pressed)
playAI :: Board -> GameStatus -> UI.Canvas -> UI (Board, GameStatus)
playAI board gameStat canvas =
    case (getNum0Opening board) of -- first find unopened neighbours of Num 0
      Just cell -> makeMove canvas board cell
      Nothing   ->
       case (findMine board) of -- if not, find an obvious mine if you can
        Just cell ->
          do
            newBoard <- manageFlagging canvas board cell
            return (newBoard, gameStat)
        Nothing   ->
          case (findSafeCell board) of -- if not, find an obvious safe move if poss
            Just cell -> makeMove canvas board cell
            Nothing   ->
              case (getHiddenCorner board) of -- if not, find an unopened corner cell
                Just cell -> makeMove canvas board cell
                Nothing   -> return (board, gameStat) -- if all above fails

-- handles attempt to reveal a cell
makeMove :: UI.Canvas -> Board -> Cell -> UI (Board, GameStatus)
makeMove canvas board (Cell (r, c) v s) =
  let newBoard = revealCell board (Cell (r, c) v s)
    in do
        cellLocation <- getCellStartPt (r, c) -- get cell location
        (newestBoard, gameStatus) <- updateBoardNStatus canvas newBoard (Cell (r, c) v s) cellLocation -- update the board and game status
        liftIO $ print $ show (Cell (r, c) v s) ++ " --- " ++ show newBoard
        return (newestBoard, gameStatus) -- return the updated board and game status wrapped by UI

-- handles attempt to flag a cell
manageFlagging :: UI.Canvas -> Board -> Cell -> UI Board
manageFlagging canvas board (Cell (r, c) v s) =
  let newBoard = flagOrUnflag board (Cell (r, c) v s)
    in do
        (xPos, yPos) <- getCellStartPt (r, c)
        handleFlaggingUI canvas (Cell (r, c) v s) (xPos, yPos)
        return newBoard

-- updates the board UI based on flagging/unflagging a cell
handleFlaggingUI :: UI.Canvas -> Cell -> UI.Point -> UI ()
handleFlaggingUI canvas (Cell (_, _) _ stat) (xPos, yPos) =
  case stat of
    Hidden  -> -- mark all flags with an F, if cell was hidden
      canvas # UI.strokeText ("F") (xPos + cellWidth/2, yPos + cellHeight/2)
    Flagged -> -- if cell was flagged, unflag it (make it hidden)
      do
        canvas # set' UI.fillStyle (UI.htmlColor "darkgray")
        canvas # UI.fillRect (xPos, yPos) cellWidth cellHeight
    _       -> -- cant flag or unflag a cell that's already revealed
      return ()

-- gets the cell number that was clicked on by the Human player (maps click coordinates to cell pos)
getClickedCellNum :: UI.Point -> (RowNum, ColNum)
getClickedCellNum (x, y) =
  (floor $ y/(yGap + cellHeight), floor $ x/(xGap + cellWidth) )

-- get the cell's top-left coordinates, just a calculation function
getCellStartPt :: (RowNum, ColNum) -> UI UI.Point
getCellStartPt (r, c) =
  let rowNum               = fromIntegral r
      colNum               = fromIntegral c
      xPos                 = (colNum + 1) * xGap + colNum * cellWidth
      yPos                 = (rowNum + 1) * yGap + rowNum * cellHeight
    in return (xPos, yPos)

-- Updates the board UI based on game outcome and cell clicked
updateBoardNStatus :: UI.Canvas -> Board -> Cell -> UI.Point -> UI (Board, GameStatus)
updateBoardNStatus canvas board cell cellLocation =
  case (updateGameStatus board cell) of
    Loss    -> do -- if it's a loss, show a loss and end the game
                depictLoss canvas cellLocation
                newBoard <- endGame board canvas
                return (newBoard, Loss)
    Win     -> do -- if it's a win, show the win and end the game
                depictWin canvas cellLocation cell
                newBoard <- endGame board canvas
                return (newBoard, Win)
    Ongoing -> do -- just reveal the cell, and carry on
                depictOpening canvas cellLocation cell
                return (board, Ongoing)

-- display a loss screen, when the player loses (shows UI for opening a mine)
depictLoss :: UI.Canvas -> UI.Point -> UI ()
depictLoss canvas cellLocation =
  do
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # UI.fillRect cellLocation cellWidth cellHeight
    canvas # UI.strokeText ("You lose.") (canvasWidth/2, canvasHeight - resultSpace/2)

-- display a win screen, when the player wins
depictWin :: UI.Canvas -> UI.Point -> Cell -> UI ()
depictWin canvas cellLocation cell =
  do
    depictOpening canvas cellLocation cell -- open cell, then say it's a win
    canvas # UI.strokeText ("You win!") (canvasWidth/2, canvasHeight - resultSpace/2)

-- show UI stuff for opening of a hidden cell, if it's not a mine
depictOpening :: UI.Canvas -> UI.Point -> Cell -> UI ()
depictOpening canvas (xPos, yPos) cell =
  case (cell) of
    Cell (_, _) (Num i) Hidden ->
      do
        canvas # set' UI.fillStyle (UI.htmlColor "white")
        canvas # UI.fillRect (xPos, yPos) cellWidth cellHeight
        canvas # UI.strokeText (show i) (xPos + cellWidth/2, yPos + cellHeight/2)
    otherwise                  -> return ()

-- puts an end to the game (by revealing all other cells)
endGame :: Board -> UI.Canvas -> UI Board
endGame board canvas = revealRemainingCells board board canvas -- pass board twice as that function needs a board copy as well

-- reveal all cells which aren't Shown yet
revealRemainingCells :: Board -> Board -> UI.Canvas -> UI Board
revealRemainingCells newBoard [] _ = return newBoard
revealRemainingCells boardCopy (cell: otherCells) canvas =
    case cell of
      Cell (_, _) _ Shown -> revealRemainingCells boardCopy otherCells canvas
      otherwise           ->
        do
          newBoard <- forceRevealCellComplete boardCopy cell canvas
          revealRemainingCells newBoard otherCells canvas

-- reveals a cell (UI and programatically) regardless of it's current status
forceRevealCellComplete :: Board -> Cell -> UI.Canvas -> UI Board
forceRevealCellComplete board (Cell (rowNum, colNum) val stat) canvas =
  let newBoard      = forceRevealCell board (Cell (rowNum, colNum) val stat)
      col           = fromIntegral colNum
      row           = fromIntegral rowNum
      xPos          = (col + 1) * xGap + col * cellWidth
      yPos          = (row + 1) * yGap + row * cellHeight
      cellLocation  = (xPos, yPos)
    in case val of
      Mine -> --mines shown in pink
        do
          canvas # set' UI.fillStyle (UI.htmlColor "pink")
          canvas # UI.fillRect cellLocation cellWidth cellHeight
          return newBoard
      (Num i) -> -- non-mines shown in white with number on top
        do
          canvas # set' UI.fillStyle (UI.htmlColor "white")
          canvas # UI.fillRect cellLocation cellWidth cellHeight
          canvas # UI.strokeText (show i) (xPos + cellWidth/2, yPos + cellHeight/2)
          return newBoard
