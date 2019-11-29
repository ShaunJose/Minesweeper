-- @author: Shaun Jose --
{-# LANGUAGE OverloadedStrings #-}
module Configs where

import CustomDataTypes

-- number of rows and columns
rows :: RowNum
rows = 9
cols :: ColNum
cols = 9

-- number of mines
mines :: Int
mines = 10

-- width and height of cell in minesweeper
cellWidth = 17.0
cellHeight = cellWidth -- keep it as a square

-- spacing between cells
xGap = 3.0
yGap = 2.0

-- extra space for win or lose message
resultSpace = 25

-- some button messages for human player click modes
revealMsg = "Change to flag mode" --message displayed when in reveal mode
flagMsg = "Change to reveal mode" --message displayed when in flag mode

-- width and height of canvas
canvasWidth = colsNum * cellWidth + (colsNum + 1) * xGap
                where colsNum = fromIntegral cols
canvasHeight = rowsNum * cellHeight + (rowsNum + 1) * yGap + resultSpace
                where rowsNum = fromIntegral rows
