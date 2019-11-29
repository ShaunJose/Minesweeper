-- @author: Shaun Jose --
{-# LANGUAGE OverloadedStrings #-}
module CustomDataTypes where

-- DATA CREATION --

-- A cell has a row number and column number (for simplicity and understanding), a Value - which could be a mine or simply a number indicating the amount of mines next to it, and a Status - whetehr it has been revealed yet or not, or even flagged!

-- Modelling the Cell --
-- Data Parts needed for describing a cell
data Value = Mine | Num Int -- Value = What's in the cell? (a mine or number?)
                deriving (Eq, Show)
data Status = Hidden | Shown | Flagged -- Status = status of cell
                deriving (Eq, Show)
type RowNum = Int -- rownumber (from 0 to rows-1)
type ColNum = Int -- columnNumber (from 0 to cols-1)

-- Cell definition
data Cell = Cell (RowNum, ColNum) Value Status
              deriving (Eq, Show)


-- A board could be a 2d list of Cells. But hey, since we have numbers to state which row and column each cell lies in, we don't really need a list of lists! That could complicate things, so I chose to represent a board by just a list of cells

-- Modelling the board --
-- Board is a list of cells
type Board = [Cell]

-- Other useful data types --
-- status of the game -> is it onGoing, or if over, what's the result?
data GameStatus = Loss | Win | Ongoing
              deriving (Eq, Show)

-- mode that human player is in -> is the Human trying to reveal or flag a mine with this left click?
data ClickMode = RevealMode | FlagMode
              deriving (Eq)

-- type of player playing the game (self-explanatory)
data PlayerType = Human | AI
