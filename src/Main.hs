-- @author: Shaun Jose --
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- imports
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import System.Random
import Data.IORef
import Control.Monad.Trans (liftIO)

-- DATA CREATION --

-- Modelling a cell in minesweeper --
data Value = Mine | Num Int -- Value = What's in the cell?
                deriving (Eq, Show)
data Status = Hidden | Shown | Flagged -- Status = status of cell
                deriving (Eq, Show)
type RowNum = Int -- rownumber (from 0 to rows-1)
type ColNum = Int -- columnNumber (from 0 to cols-1)
data Cell = Cell (RowNum, ColNum) Value Status  -- Cell definition --
              deriving (Eq, Show)

-- Modelling the board --
-- Board is a list of cells
type Board = [Cell]

--status of the game
data GameStatus = Loss | Win | Ongoing
              deriving (Eq, Show)

-- FUNCS --

-- applies function to neighbouring cells of cell provided, in the board
gridMap :: Board -> (Cell -> Cell) -> Cell -> Board
gridMap [] _ _ = []
gridMap ((Cell (row, col) val status) :board) func cell =
  let cellNums = gridList cell
    in case (isMember (row, col) cellNums) of
      True  -> (func (Cell (row, col) val status)) : gridMap board func cell
      False -> (Cell (row, col) val status) : gridMap board func cell

-- returns a list of locations of neighbouring-cells (exclusive of centre cell's location) NOTE that rows and cols returned may be out of bounds
gridList :: Cell -> [(RowNum, ColNum)]
gridList (Cell (row, col) _ _) =
  [(row-1, col-1), (row-1, col), (row-1, col+1),
   (row, col-1), {- (row, col), -} (row, col+1),
   (row+1, col-1), (row+1, col), (row+1, col+1)]

-- increment the Num value of a cell
incrCellVal :: Cell -> Cell
incrCellVal (Cell (r, c) (Num i) status) = Cell (r, c) (Num (i+1)) status
incrCellVal cell = cell -- for mine-cell cases

-- Creating the board --
-- Create the board with all cells Hidden, and place chosen Mines
createBoard :: RowNum -> ColNum -> RowNum -> [(RowNum, ColNum)] -> Board
createBoard rows cols currRow mines | rows == currRow = []
createBoard rows cols currRow mines =
  (createRow currRow cols 0 mines) ++ (createBoard rows cols (currRow + 1) mines)

-- create a row (a list of cells), intialised all to Num 0 and Hidden
-- NOTE: This function also places the mines in the appropriate cells
createRow :: RowNum -> ColNum -> ColNum -> [(RowNum, ColNum)] -> Board
createRow row cols currCol mines | cols == currCol = []
createRow row cols currCol mines =
  let cellNum = (row, currCol)
    in case (isMember cellNum mines) of
      False -> Cell cellNum (Num 0) Hidden : createRow row cols (currCol+1) mines
      True  -> Cell cellNum Mine Hidden : createRow row cols (currCol+1) mines

-- random Int generator (within bounds)
makeRandomInt :: StdGen -> (Int, Int) -> (Int, StdGen)
makeRandomInt g bounds = randomR bounds g

-- random Int tuple generator (within certain bounds)
makeRandIntTuple :: StdGen -> (Int, Int) -> (Int, Int) -> ((Int, Int), StdGen)
makeRandIntTuple g bounds1 bounds2 =
              let firstRes = makeRandomInt g bounds1
                in let secondRes = makeRandomInt (snd firstRes) bounds2
                  in ((fst firstRes, fst secondRes ), snd secondRes)

-- random Int tuple list generator (with no duplicate tuples)
-- NOTE: This GENERATES RANDOM MINES' LOCATIONS (unique mine locations)
randIntTupleList :: StdGen -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> ( [(Int, Int)], StdGen)
randIntTupleList g (_, _) (_, _) currLst 0 = (currLst, g)
randIntTupleList g bounds1 bounds2 currLst count =
  let randRes = makeRandIntTuple g bounds1 bounds2 -- get (randIntTuple, new StdGen)
    in let tuple = fst randRes -- get randIntTuple
      in let gen = snd randRes -- get the new StdGen
        in case (isMember tuple currLst) of
          False -> randIntTupleList gen bounds1 bounds2 (tuple:currLst) (count-1)
          True  -> randIntTupleList gen bounds1 bounds2 currLst count

-- chooses random mines based on rows and columns
chooseMines :: StdGen -> Int -> Int -> [(Int, Int)] -> Int -> ( [(Int, Int)], StdGen)
chooseMines g rows cols _ mines =
  randIntTupleList g (0, rows-1) (0, cols-1) [] mines

-- checks if in alement exists in a list
isMember :: (Eq a) => a -> [a] -> Bool
isMember _ []        = False
isMember elem (x:xs) = case (elem == x) of
                        True  -> True
                        False -> isMember elem xs

-- fills board cells with values, given board with place mines
fillBoard :: Board -> Board -> Board
fillBoard boardCopy [] = boardCopy
fillBoard boardCopy (cell:board)
  | isMine cell = fillBoard (gridMap boardCopy incrCellVal cell) board --update boardCopy and go to next cell
  | otherwise    = fillBoard boardCopy board -- go to next cell

-- True if cell has a mine, False otherwise
isMine :: Cell -> Bool
isMine (Cell (_, _) Mine _)    = True
isMine _                       = False

-- modify board to reveal/uncover a cell if you can (if it's hidden)
revealCell :: Board -> Cell -> Board
revealCell board (Cell (row, col) val Hidden)
  | val /= (Num 0) = replaceElem board (Cell (row, col) val Hidden) (revealSingleCell $ Cell (row, col) val Hidden)
  | otherwise = gridMap newBoard revealSingleCell (Cell (row, col) val Hidden)
    where
      newBoard = replaceElem board (Cell (row, col) val Hidden) (revealSingleCell $ Cell (row, col) val Hidden)

-- reveal/uncover a cell if you can (if it's hidden)
revealSingleCell :: Cell -> Cell
revealSingleCell (Cell (row, col) val Hidden) = (Cell (row, col) val Shown)
revealSingleCell cell = cell

-- flag a cell if you can (if it's Hidden)
flagCell :: Board -> Cell -> Board
flagCell board (Cell (row, col) val Hidden) =
            replaceElem board (Cell (row, col) val Hidden) (Cell (row, col) val Flagged)
flagCell board _ = board

-- checks if the game ended (board = latest updated board) (cell = cell chosen)
updateGameStatus :: Board -> Cell -> GameStatus
updateGameStatus board cell
  | isMine cell             = Loss --NOTE: can do without cell (use only board)
  | safeCellsRevealed board = Win
  | otherwise               = Ongoing

-- checks if all safe cells are revealed
safeCellsRevealed :: Board -> Bool
safeCellsRevealed [] = True
safeCellsRevealed (cell:board) =
  case (cell) of
    (Cell (_, _) (Num i) Hidden) -> False
    otherwise                    -> safeCellsRevealed board

-- replaces an element in a list with another element
replaceElem :: (Show a, Eq a) => [a] -> a -> a -> [a]
replaceElem [] _ _ = []
replaceElem (currElem : t) oldElem newElem
  | oldElem == currElem = newElem : replaceElem t oldElem newElem
  | otherwise           = currElem : replaceElem t oldElem newElem

trial :: [a] -> [a] -> [a]
trial [] newLst = newLst
trial (currElem:lst) newLst = trial lst (currElem:newLst)

-- initialise game --
initGame :: Int -> IO ()
initGame 0 = getChar >>= putChar
initGame 1 = getChar >>= putChar
initGame n   = getChar >>= putChar

getNum0Cell ((Cell (row, col) (Num 0) status) : _) = (Cell (row, col) (Num 0) status)
getNum0Cell (cell:board) = getNum0Cell board

-- finds the cell in a baord based on rownum and colnum
findCell :: Board -> (RowNum, ColNum) -> Cell
findCell [] (row, col) = error $ "findCell: cell not found in board" ++ show row ++ ", " ++  show col
findCell ((Cell (r, c) val stat): board) (row, col) =
  case (r == row && c == col) of
    True  -> Cell (r, c) val stat
    False -> findCell board (row, col)

-- Main
-- main = do
--         print $ Cell (0,1) (Num 1) Hidden == Cell (0,1) (Num 1) Hidden --Eq test
--         print $ Cell (0,1) (Num 1) Hidden == Cell (1,1) (Num 1) Hidden --Eq test
--         g <- getStdGen
--         print $ fst $ makeRandomInt g (1, 7) -- makeRandomInt test
--         print $ fst $ makeRandIntTuple g (1, 7) (1, 4) -- makeRandIntTuple test
--         print $ randIntTupleList g (1, 2) (1, 4) [] 4 -- randomTupleList test
--         print $ createRow 1 4 0 (fst $ chooseMines g 10 4 [] 4) --createRow with mines Test
--         print $ createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 4) --create Board with mines Test
--         print $ gridList (Cell (2,1) (Num 0) Hidden) -- gridList test
--         print $ incrCellVal (Cell (2,1) (Num 0) Hidden) -- incr Cell val test
--         print $ gridMap (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 4)) incrCellVal (Cell (2,1) (Num 0) Hidden) -- gridMap + incrElem test
--         print $ isMine (Cell (2,1) (Num 0) Hidden) -- isMine test
--         print $ isMine (Cell (2,1) Mine Hidden) -- isMine test
--         print $ fillBoard (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10)) (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10)) -- fillBoard test
--         print $ replaceElem [1,2,3,4,5,1,421,52,13] 421 62 --replaceElem test
        -- print $ revealCell (fillBoard (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10)) (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10))) (getNum0Cell (fillBoard (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10)) (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10))))

main :: IO ()
main = do
  g <- getStdGen
  print $ fillBoard (createBoard rows cols 0 (fst $ chooseMines g rows cols [] 10)) (createBoard rows cols 0 (fst $ chooseMines g rows cols [] 10))
  startGUI defaultConfig uiSetup

-- number of rows and columns TODO: decide either pass in or this method and change everywhere, accordingly
rows = 10
cols = 4
-- width and height of cell in minesweeper
cellWidth = 17.0
cellHeight = cellWidth -- keep it as a square
-- spacing between cells
xGap = 3.0
yGap = 2.0
-- width and height of canvas
canvasWidth = colsNum * cellWidth + (colsNum + 1) * xGap
                where colsNum = fromIntegral cols
canvasHeight = rowsNum * cellHeight + (rowsNum + 1) * yGap
                where rowsNum = fromIntegral rows

-- sets up the UI for the game
uiSetup :: Window -> UI ()
uiSetup window = do
    return window # set title "Minesweeper_AI"

    canvas <- UI.canvas
      # set UI.height (ceiling canvasHeight)
      # set UI.width (ceiling canvasWidth)
      # set UI.style [("border", "solid black 1px"), ("background", "lightgray")]

    clear <- UI.button
       #+ [string "Clear"]
    fillButton <- UI.button
       #+ [string "Fill board"]

    coord <- liftIO $ newIORef (0,0)

    getBody window #+
      [row [element canvas], element clear, element fillButton]

    on UI.mousemove canvas $ \(x,y) ->
      do liftIO $ writeIORef coord (x,y)
    on UI.click canvas $ \_ ->
      do
        (x, y) <- liftIO $ readIORef coord
        g      <- liftIO $ getStdGen
        respond (fillBoard (createBoard rows cols 0 (fst $ chooseMines g rows cols [] 10)) (createBoard rows cols 0 (fst $ chooseMines g rows cols [] 10))) canvas (fromIntegral x, fromIntegral y)
    on UI.click clear $ const $
      canvas # UI.clearCanvas
    on UI.click fillButton $ \_ ->
      do
        canvas # set' UI.fillStyle (UI.htmlColor "darkgray")
        createBoardUI (0.0, 0.0) rows cols canvas

-- creates the board UI
createBoardUI :: UI.Point -> RowNum -> ColNum -> UI.Canvas -> UI ()
createBoardUI (xPos, yPos) rows cols canvas | rows < 1 =
                                                error "createBoardUI: rows < 1"
createBoardUI (xPos, yPos) 1 cols canvas =
    createRowUI (xPos + xGap, yPos + yGap) cols canvas
createBoardUI (xPos, yPos) rows cols canvas =
  do
    createRowUI (xPos + xGap, yPos + yGap) cols canvas
    createBoardUI (xPos, yPos + yGap + cellHeight) (rows - 1) cols canvas

-- creates the UI for a row of cells
createRowUI :: UI.Point -> ColNum -> UI.Canvas -> UI ()
createRowUI coord cols canvas | cols < 1 = error "createRowUI: cols < 1"
createRowUI coord 1 canvas = canvas # UI.fillRect coord cellWidth cellHeight
createRowUI (xPos, yPos) cols canvas =
  do
    canvas # UI.fillRect (xPos, yPos) cellWidth cellHeight
    createRowUI (xPos + cellWidth + xGap, yPos) (cols - 1) canvas

-- responds to click on the canvas
respond :: Board -> UI.Canvas -> UI.Point -> UI () --TODO: pass in board
respond board canvas coord =
  let (rowIndex, colIndex) = getClickedCellNum coord
      cellClicked = findCell board (rowIndex, colIndex)
      newBoard = revealCell board cellClicked
      rowNum = fromIntegral rowIndex
      colNum = fromIntegral colIndex
      xPos = (colNum + 1) * xGap + colNum * cellWidth
      yPos = (rowNum + 1) * yGap + rowNum * cellHeight
      cellLocation = (xPos, yPos)
    in case (updateGameStatus newBoard cellClicked) of
      Loss    -> do
                  canvas # set' UI.fillStyle (UI.htmlColor "red")
                  canvas # UI.fillRect cellLocation cellWidth cellHeight
                  liftIO $ print $ show cellClicked ++ " --- " ++ show newBoard
      Win     -> do
                  canvas # set' UI.fillStyle (UI.htmlColor "green")
                  canvas # UI.fillRect (0.0, 0.0) canvasWidth canvasHeight
                  liftIO $ print $ show cellClicked ++ " --- " ++ show newBoard
      Ongoing -> do
                  canvas # set' UI.fillStyle (UI.htmlColor "white")
                  canvas # UI.fillRect cellLocation cellWidth cellHeight
                  liftIO $ print $ show cellClicked ++ " --- " ++ show newBoard


-- gets the cell number clicked on
getClickedCellNum :: UI.Point -> (RowNum, ColNum)
getClickedCellNum (x, y) =
  (floor $ y/(yGap + cellHeight), floor $ x/(xGap + cellWidth) )
