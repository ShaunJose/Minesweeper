-- @author: Shaun Jose --
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- imports
import System.Random

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
  | hasMine cell = fillBoard (gridMap boardCopy incrCellVal cell) board --update boardCopy and go to next cell
  | otherwise    = fillBoard boardCopy board -- go to next cell

-- True if cell has a mine, False otherwise
hasMine :: Cell -> Bool
hasMine (Cell (_, _) Mine _)    = True
hasMine _                       = False

-- modify board to reveal/uncover a cell if you can (if it's hidden)
revealCell :: Board -> Cell -> Board
revealCell board (Cell (row, col) val Hidden)
  | val /= (Num 0) = replaceElem board (Cell (row, col) val Hidden) (revealSingleCell $ Cell (row, col) val Hidden)
  | otherwise = gridMap newBoard revealSingleCell (Cell (row, col) val Hidden)
    where
      newBoard = replaceElem board (Cell (row, col) val Hidden) (revealSingleCell $ Cell (row, col) val Hidden)
revealCell board _ = board

-- reveal/uncover a cell if you can (if it's hidden)
revealSingleCell :: Cell -> Cell
revealSingleCell (Cell (row, col) val Hidden) = (Cell (row, col) val Shown)
revealSingleCell cell = cell

-- flag a cell if you can (if it's Hidden)
flagCell :: Board -> Cell -> Board
flagCell board (Cell (row, col) val Hidden) =
            replaceElem board (Cell (row, col) val Hidden) (Cell (row, col) val Flagged)
flagCell board _ = board

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

-- Main
main = do
        print $ Cell (0,1) (Num 1) Hidden == Cell (0,1) (Num 1) Hidden --Eq test
        print $ Cell (0,1) (Num 1) Hidden == Cell (1,1) (Num 1) Hidden --Eq test
        g <- getStdGen
        print $ fst $ makeRandomInt g (1, 7) -- makeRandomInt test
        print $ fst $ makeRandIntTuple g (1, 7) (1, 4) -- makeRandIntTuple test
        print $ randIntTupleList g (1, 2) (1, 4) [] 4 -- randomTupleList test
        print $ createRow 1 4 0 (fst $ chooseMines g 10 4 [] 4) --createRow with mines Test
        print $ createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 4) --create Board with mines Test
        print $ gridList (Cell (2,1) (Num 0) Hidden) -- gridList test
        print $ incrCellVal (Cell (2,1) (Num 0) Hidden) -- incr Cell val test
        print $ gridMap (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 4)) incrCellVal (Cell (2,1) (Num 0) Hidden) -- gridMap + incrElem test
        print $ hasMine (Cell (2,1) (Num 0) Hidden) -- hasMine test
        print $ hasMine (Cell (2,1) Mine Hidden) -- hasMine test
        print $ fillBoard (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10)) (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10)) -- fillBoard test
        print $ replaceElem [1,2,3,4,5,1,421,52,13] 421 62 --replaceElem test
        print $ revealCell (fillBoard (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10)) (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10))) (getNum0Cell (fillBoard (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10)) (createBoard 10 4 0 (fst $ chooseMines g 10 4 [] 10))))
