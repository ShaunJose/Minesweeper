-- @author: Shaun Jose --
{-# LANGUAGE OverloadedStrings #-}
module CustomDataTypes where

-- DATA CREATION --

-- Modelling the Cell --
-- Data Parts needed for decribing a cell
data Value = Mine | Num Int -- Value = What's in the cell?
                deriving (Eq, Show)
data Status = Hidden | Shown | Flagged -- Status = status of cell
                deriving (Eq, Show)
type RowNum = Int -- rownumber (from 0 to rows-1)
type ColNum = Int -- columnNumber (from 0 to cols-1)

-- Cell definition
data Cell = Cell (RowNum, ColNum) Value Status
              deriving (Eq, Show)

-- Modelling the board --
-- Board is a list of cells
type Board = [Cell]

-- Other useful data types --
-- status of the game
data GameStatus = Loss | Win | Ongoing
              deriving (Eq, Show)

-- mode that human player is in
data ClickMode = RevealMode | FlagMode
              deriving (Eq)

-- type of player playing the game
data PlayerType = Human | AI
