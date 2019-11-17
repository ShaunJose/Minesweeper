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

--TODO make random Int generator (within bounds)
--TODO make random tuple generator

-- initialise game --
initGame :: Int -> IO ()
initGame 0 = getChar >>= putChar
initGame 1 = getChar >>= putChar
initGame n   = getChar >>= putChar

main = do
        print $ createRow 1 4 0
        g <- getStdGen
        print $ Cell (0,1) (Num 1) Hidden == Cell (0,1) (Num 1) Hidden
        print $ Cell (0,1) (Num 1) Hidden == Cell (1,1) (Num 1) Hidden
