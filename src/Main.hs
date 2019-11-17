-- @author: Shaun Jose --
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- imports
import System.Random

-- Modelling a cell in minesweeper --
data Element = Mine | Num Int -- Element = What's in the cell?
                deriving Show
data Status = Shown | Hidden | Flagged -- Status = status of cell
                deriving Show
type RowNum = Int -- rownumber (from 0 to m-1)
type ColNum = Int -- columnNumber (from 0 to n-1)
data Cell = Cell (RowNum, ColNum) Element Status  -- Cell definition --
              deriving Show

-- Board is a list of rows, which is a list of cells
type Row = [Cell]
type Board = [[Cell]]


-- create a row (a list of cells), intialised all to Num 0 and Hidden
createRow :: RowNum -> ColNum -> ColNum -> Row
createRow row n currCol | n == currCol = []
createRow row n currCol = Cell (row, currCol) (Num 0) Hidden : createRow row n (currCol+1)

-- [ [Cell (0, 0), Cell (0, 1), Cell (0, 2) Cell (0, 3)],
--   [Cell (1, 0), Cell (1, 1), Cell (1, 2) Cell (1, 3)],
--   [Cell (2, 0), Cell (2, 1), Cell (2, 2) Cell (2, 3)] ]

-- createBoard rows cols | n < 0 = []
-- createBoard n =

-- initialise game --
initGame :: Int -> IO ()
initGame 0 = getChar >>= putChar
initGame 1 = getChar >>= putChar
initGame n   = getChar >>= putChar

main = print $ createRow 1 4 0
