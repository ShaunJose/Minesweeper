-- @author: Shaun Jose --
{-# LANGUAGE OverloadedStrings #-}
module MinesweeperFuncs where

-- contains all the non ui minesweeper funcs

import CustomDataTypes
import System.Random
import Configs

-- FUNCS --

-- Choosing the Mines --
-- chooses random mines based on rows and columns
chooseMines :: StdGen -> Int -> Int -> [(Int, Int)] -> Int -> ( [(Int, Int)], StdGen)
chooseMines g rows cols _ mines =
  randIntTupleList g (0, rows-1) (0, cols-1) [] mines

-- random Int tuple list generator (with no duplicate tuples)
-- NOTE: This generates random mines' locations (UNIQUE mine locations)
randIntTupleList :: StdGen -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> ( [(Int, Int)], StdGen)
randIntTupleList g (_, _) (_, _) currLst 0 = (currLst, g)
randIntTupleList g bounds1 bounds2 currLst count =
  let randRes = makeRandIntTuple g bounds1 bounds2 -- get (randIntTuple, new StdGen)
      tuple = fst randRes -- get randIntTuple
      gen   = snd randRes -- get the new StdGen
    in case (isMember tuple currLst) of
      False -> randIntTupleList gen bounds1 bounds2 (tuple:currLst) (count-1)
      True  -> randIntTupleList gen bounds1 bounds2 currLst count

-- random Int tuple generator (within certain bounds)
makeRandIntTuple :: StdGen -> (Int, Int) -> (Int, Int) -> ((Int, Int), StdGen)
makeRandIntTuple g bounds1 bounds2 =
              let firstRes = makeRandomInt g bounds1
                  secondRes = makeRandomInt (snd firstRes) bounds2
                in ((fst firstRes, fst secondRes ), snd secondRes)

-- random Int generator (within bounds)
makeRandomInt :: StdGen -> (Int, Int) -> (Int, StdGen)
makeRandomInt g bounds = randomR bounds g


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

-- fills board cells with values, given board with place mines
fillBoard :: Board -> Board -> Board
fillBoard boardCopy [] = boardCopy
fillBoard boardCopy (cell:board)
  | isMine cell = fillBoard (gridMap boardCopy incrCellVal cell) board --update boardCopy and go to next cell
  | otherwise    = fillBoard boardCopy board -- go to next cell


-- Simple Boolean Funcs for Cells (can be used for filter easily) --
-- True if cell has a mine, False otherwise
isMine :: Cell -> Bool
isMine (Cell (_, _) Mine _)    = True
isMine _                       = False

-- True if cell is hidden, false otherwise
isHidden :: Cell -> Bool
isHidden (Cell (_, _) _ Hidden)    = True
isHidden _                         = False

-- True if cell is Shown, false otherwise
isShown :: Cell -> Bool
isShown (Cell (_, _) _ Shown)     = True
isShown _                         = False

-- True if cell is Flagged, false otherwise
isFlagged :: Cell -> Bool
isFlagged (Cell (_, _) _ Flagged)     = True
isFlagged _                           = False

-- True if cell has value of Num 0, false otherwise
isNum0 :: Cell -> Bool
isNum0 (Cell (_, _) (Num 0) _)     = True
isNum0 _                           = False

-- True is cell is in the row specified
isInRow :: RowNum -> Cell -> Bool
isInRow row (Cell (r, _) _ _) = row == r

-- True is cell is in the column specified
isInCol :: ColNum -> Cell -> Bool
isInCol col (Cell (_, c) _ _) = col == c


-- Grid-Based Functions (grid = block of 9 cells) --
-- applies (Cell -> Cell) func to neighbouring cells of cell provided, in the board
gridMap :: Board -> (Cell -> Cell) -> Cell -> Board
gridMap [] _ _ = []
gridMap ((Cell (row, col) val status) :board) func cell =
  let cellNums = gridList cell
    in case (isMember (row, col) cellNums) of
      True  -> (func (Cell (row, col) val status)) : gridMap board func cell
      False -> (Cell (row, col) val status) : gridMap board func cell

-- returns a list of locations of neighbouring-cells (excluding centre cell's location)
-- NOTE that rows and cols returned may be out of bounds
gridList :: Cell -> [(RowNum, ColNum)]
gridList (Cell (row, col) _ _) =
  [(row-1, col-1), (row-1, col), (row-1, col+1),
   (row, col-1), {- (row, col), -} (row, col+1),
   (row+1, col-1), (row+1, col), (row+1, col+1)]

-- checks if a cell with (RowNum, ColNum) must exist in the board
inRange :: (RowNum, ColNum) -> Bool
inRange (r, c)
  | r >= 0 && c >= 0 && r < rows && c < cols = True
  | otherwise                                = False

-- returns a list of locations of neighbouring-cells (excluding centre cell's location) IF THEY ARE VALID LOCATIONS
validGridList :: Cell -> [(RowNum, ColNum)]
validGridList cell = filter inRange (gridList cell)

-- get neighbouring cells of the cell provided, from the board
getNeighbourCells :: Board -> Cell -> [Cell]
getNeighbourCells board cell =
  let validTuples = validGridList cell
    in map (findCell board) validTuples


-- Revealing or Flagging Cells --
-- modify board to reveal/uncover a cell if you can (if it's hidden)
revealCell :: Board -> Cell -> Board
revealCell board (Cell (row, col) val Hidden)
  | val /= (Num 0) = replaceElem board (Cell (row, col) val Hidden) (revealSingleCell $ Cell (row, col) val Hidden)
  | otherwise = replaceElem board (Cell (row, col) val Hidden) (revealSingleCell $ Cell (row, col) val Hidden)
revealCell board _ = board

-- reveal/uncover a cell if you can (if it's hidden)
revealSingleCell :: Cell -> Cell
revealSingleCell (Cell (row, col) val Hidden) = (Cell (row, col) val Shown)
revealSingleCell cell = cell

-- modify board to reveal/uncover a cell regardless of it's status
forceRevealCell :: Board -> Cell -> Board
forceRevealCell board (Cell (row, col) val stat) =
  replaceElem board (Cell (row, col) val stat) (Cell (row, col) val Shown)

-- flag a cell if it's Hidden, unflag if if it's flagged
flagOrUnflag :: Board -> Cell -> Board
flagOrUnflag board (Cell (row, col) val Hidden) =
            replaceElem board (Cell (row, col) val Hidden) (Cell (row, col) val Flagged)
flagOrUnflag board (Cell (row, col) val Flagged) =
            replaceElem board (Cell (row, col) val Flagged) (Cell (row, col) val Hidden)
flagOrUnflag board _ = board


-- MISC Utility Functions --
-- increment the Num value of a cell
incrCellVal :: Cell -> Cell
incrCellVal (Cell (r, c) (Num i) status) = Cell (r, c) (Num (i+1)) status
incrCellVal cell = cell -- for mine-cell cases

-- checks if in alement exists in a list
isMember :: (Eq a) => a -> [a] -> Bool
isMember _ []        = False
isMember elem (x:xs) = case (elem == x) of
                        True  -> True
                        False -> isMember elem xs

-- checks if the game ended (board = latest updated board) (cell = cell chosen)
updateGameStatus :: Board -> Cell -> GameStatus
updateGameStatus board (Cell (row, col) val stat)
  | stat == Flagged                   = Ongoing
  | isMine (Cell (row, col) val stat) = Loss
  | safeCellsRevealed board           = Win
  | otherwise                         = Ongoing

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

-- finds the cell in a baord based on rownum and colnum
findCell :: Board -> (RowNum, ColNum) -> Cell
findCell [] (row, col) = error $ "findCell: cell not found in board" ++ show row ++ ", " ++  show col
findCell ((Cell (r, c) val stat): board) (row, col) =
  case (r == row && c == col) of
    True  -> Cell (r, c) val stat
    False -> findCell board (row, col)


-- AI Logic functions --
-- gets a hidden corner cell if there is one
getHiddenCorner :: Board -> Maybe Cell
getHiddenCorner [] = Nothing
getHiddenCorner board =
  case findCell board (0, 0) of -- top left
    Cell (r, c) v Hidden -> Just (Cell (r, c) v Hidden)
    _ -> case findCell board (0, cols - 1) of -- top right
      Cell (r, c) v Hidden -> Just (Cell (r, c) v Hidden)
      _ -> case findCell board (rows-1, 0) of -- bot left
        Cell (r, c) v Hidden -> Just (Cell (r, c) v Hidden)
        _ -> case findCell board (rows-1, cols-1) of -- bot right
          Cell (r, c) v Hidden -> Just (Cell (r, c) v Hidden)
          otherwise            -> Nothing -- all corners opened/flagged

-- returns a (hidden) mine
findMine :: Board -> Maybe Cell
findMine [] = Nothing
findMine board =
  let shownCells = getAllShownCells board -- gets all revealed cells
    in case (findMines board shownCells) of -- get all mines found
      []        -> Nothing
      (cell: _) -> Just cell

-- returns a list of (hidden) mines to flag
findMines :: Board -> [Cell] -> [Cell]
findMines [] _ = []
findMines _ [] = []
findMines board (cell: otherCells) =
  let (num, flaggedCount, hiddenCells) = getNumFlaggedAndCells board cell
    in if num == 0
        then findMines board otherCells -- no mines around Num 0 ! So skip
        else case (num - flaggedCount == (length hiddenCells) && not (null hiddenCells)) of -- if the opened neighbours are all mines
              True     ->  hiddenCells
              False    ->  findMines board otherCells

-- returns a safe (hidden) cell to open up
findSafeCell :: Board -> Maybe Cell
findSafeCell [] = Nothing
findSafeCell board =
  let shownCells = getAllShownCells board
    in case (findSafeCells board shownCells) of
      []        -> Nothing
      (cell: _) -> Just cell

-- returns a list of safe (hidden) cells to open up
findSafeCells :: Board -> [Cell] -> [Cell]
findSafeCells [] _ = []
findSafeCells _ [] = []
findSafeCells board (cell: otherCells) =
  let (num, flaggedCount, hiddenCells) = getNumFlaggedAndCells board cell
    in if num == 0
        then findSafeCells board otherCells -- no mines around Num 0 ! So skip
        else case (num == flaggedCount && (not (null hiddenCells))) of -- if all mines are found and there are some unopened hidden cells
              True     ->  hiddenCells
              False    ->  findSafeCells board otherCells

-- gets the cell number value, length of flagged cells and hiddenCells around it
getNumFlaggedAndCells :: Board -> Cell -> (Int, Int, [Cell])
getNumFlaggedAndCells board (Cell (r, c) (Num i) s) =
  let neighbours   = getNeighbourCells board (Cell (r, c) (Num i) s)
      flaggedCells = filter isFlagged neighbours
      hiddenCells  = filter isHidden neighbours
    in (i, length flaggedCells, hiddenCells)
getNumFlaggedAndCells board cell = --in case of a mine cell
  let neighbours   = getNeighbourCells board cell
      flaggedCells = filter isFlagged neighbours
      hiddenCells  = filter isHidden neighbours
    in (-1, length flaggedCells, hiddenCells)

-- return a HIDDEN neighbour of a shown num 0 cell, if it exists
getNum0Opening :: Board -> Maybe Cell
getNum0Opening [] = Nothing
getNum0Opening board =
  let num0ShownList  = getAllNum0ShownCells board
      neighbourCells = concat $ map (getNeighbourCells board) num0ShownList
      hiddenCells    = filter isHidden neighbourCells
    in case hiddenCells of
      []        -> Nothing
      (cell: _) -> Just cell

-- returns a hidden cell on the edge of the board
findHiddenEdge :: Board -> Maybe Cell
findHiddenEdge [] = Nothing
findHiddenEdge board =
  let topEdge = filter (isInRow 0) board
      botEdge = filter (isInRow (rows - 1)) board
      leftEdge = filter (isInCol 0) board
      rightEdge = filter (isInCol (cols - 1)) board
      hiddenEdges = filter isHidden (topEdge ++ botEdge ++ leftEdge ++ rightEdge)
    in case hiddenEdges of
      []        -> Nothing
      (cell:_)  -> Just cell

-- returns the first hidden cell it finds on the board
findHiddenCell :: Board -> Maybe Cell
findHiddenCell [] = Nothing
findHiddenCell board =
  let allHiddenCells = filter isHidden board -- get all hidden cells
    in case allHiddenCells of -- if it's empty, return nothing
      []        -> Nothing
      (cell: _) -> Just cell

-- AI Logic helpers --
-- returns all cells from a list of cells, that are shown
getAllShownCells :: [Cell] -> [Cell]
getAllShownCells cells = filter isShown cells

-- gets all revealed cells with the value of (Num 0)
getAllNum0ShownCells :: Board -> [Cell]
getAllNum0ShownCells board = filter isNum0 (getAllShownCells board)
