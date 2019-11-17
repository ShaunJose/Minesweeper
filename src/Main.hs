-- @author: Shaun Jose --
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- imports
import System.Random

-- Modelling a cell in minesweeper --
data Value = Mine | Num Int -- Value = What's in the cell?
                deriving (Eq, Show)
data Status = Hidden | Shown | Flagged -- Status = status of cell
                deriving (Eq, Show)
type RowNum = Int -- rownumber (from 0 to rows-1)
type ColNum = Int -- columnNumber (from 0 to cols-1)
data Cell = Cell (RowNum, ColNum) Value Status  -- Cell definition --
              deriving (Eq, Show)

-- Modeeling the board --
-- Board is a list of rows, which is a list of cells
type Row = [Cell]
type Board = [[Cell]]

-- Creating the board --
-- Create the board with all cells Hidden, and place chosen Mines
-- createBoard rows cols | n < 0 = []
-- createBoard n =

-- create a row (a list of cells), intialised all to Num 0 and Hidden
createRow :: RowNum -> ColNum -> ColNum -> Row
createRow row n currCol | n == currCol = []
createRow row n currCol = Cell (row, currCol) (Num 0) Hidden : createRow row n (currCol+1)

-- [ [Cell (0, 0), Cell (0, 1), Cell (0, 2) Cell (0, 3)],
--   [Cell (1, 0), Cell (1, 1), Cell (1, 2) Cell (1, 3)],
--   [Cell (2, 0), Cell (2, 1), Cell (2, 2) Cell (2, 3)] ]

-- random Int generator (within bounds)
makeRandomInt :: StdGen -> (Int, Int) -> (Int, StdGen)
makeRandomInt g bounds = randomR bounds g

-- random Int tuple generator (within certain bounds)
makeRandIntTuple :: StdGen -> (Int, Int) -> ((Int, Int), StdGen)
makeRandIntTuple g bounds =
              let firstRes = makeRandomInt g bounds
                in let secondRes = makeRandomInt (snd firstRes) bounds
                  in ((fst firstRes, fst secondRes ), snd secondRes)

-- initialise game --
initGame :: Int -> IO ()
initGame 0 = getChar >>= putChar
initGame 1 = getChar >>= putChar
initGame n   = getChar >>= putChar

main = do
        print $ createRow 1 4 0 --create Row Test
        print $ Cell (0,1) (Num 1) Hidden == Cell (0,1) (Num 1) Hidden --Eq test
        print $ Cell (0,1) (Num 1) Hidden == Cell (1,1) (Num 1) Hidden --Eq test
        g <- getStdGen
        print $ fst $ makeRandomInt g (1,7) -- makeRandomInt test
        print $ fst $ makeRandIntTuple g (1,7) -- makeRandIntTuple test
