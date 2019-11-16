-- @author: Shaun Jose --
{-# LANGUAGE OverloadedStrings #-}
module Main where

--imports
import System.Random

-- Modelling a cell in minesweeper --
type RowNum = Int
type ColNum = Int
data Cell = Cell (RowNum, ColNum)
              deriving Show

-- Board is a list of rows, which is a list of cells
type Row = [Cell]
type Board = [[Cell]]

createRow :: RowNum -> ColNum -> ColNum -> Row
createRow row n currCol | n == currCol = []
createRow row n currCol = Cell (row, currCol) : createRow row n (currCol+1)

-- [ [Cell (0, 0), Cell (0, 1), Cell (0, 2) Cell (0, 3)],
--   [Cell (1, 0), Cell (1, 1), Cell (1, 2) Cell (1, 3)],
--   [Cell (2, 0), Cell (2, 1), Cell (2, 2) Cell (2, 3)] ]

createBoard rows cols | n < 0 = []
-- createBoard n =

-- initialise game --
initGame :: Int -> IO ()
initGame 0 = getChar >>= putChar
initGame 1 = getChar >>= putChar
initGame n   = getChar >>= putChar

main = print $ createRow 1 4 0
