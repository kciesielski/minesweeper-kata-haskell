module Minesweeper.Core where	
import Data.List.Split

type Set a = a -> Bool
newEmpty :: Set a
newEmpty = \_ -> False
add :: Eq a => Set a -> a -> Set a
add s e = \i -> (e == i) || contains s i
contains :: Set a -> a -> Bool
contains s e = s e
singleton :: Eq a => a -> Set a
singleton a = \e -> (a == e)
union :: Set a -> Set a -> Set a
union a b = \e -> a e || b e

class Moveable a where
	dx :: a -> Int -> a
	dy :: a -> Int -> a

data Point = Point { x:: Int, y::Int } deriving (Eq, Show)

type MineSet = Set Point

data WorldRectangle = WorldRectangle { x1:: Int, y1:: Int, x2:: Int, y2:: Int } deriving (Show)

instance Moveable Point where
	dx point val = Point { x = (x point) + val, y = y point }
	dy point val = Point { x = x point, y = (y point) + val }

instance Show MineSet where
	show set = "set" -- how to effectively print content of a functional set? 

type TileSet = Point -> Int
addTile :: TileSet -> Point -> Int -> TileSet
emptyTileSet :: TileSet
emptyTileSet = \_ -> -1
addTile set point int = \p -> if (x p == x point && y p == y point) then int else set p

data World = World {bounds:: WorldRectangle, mines:: MineSet, tiles:: TileSet}

withinWorld :: WorldRectangle -> Point -> Bool
withinWorld rect = \point -> x1 rect <= x point && x2 rect >= x point && y1 rect <= y point && y2 rect >= y point

type Move = Point -> Point
moveUp :: Move
moveDown :: Move
moveLeft :: Move
moveRight :: Move
moveRightUp :: Move
moveRightDown :: Move
moveLeftUp :: Move
moveLeftDown :: Move
moveUp point = dy point (-1)
moveLeft point = dx point (-1)
moveRight point = dx point 1
moveDown point = dy point 1
moveRightUp point = moveRight (moveUp point)
moveLeftDown point = moveLeft (moveDown point)
moveRightDown point = moveRight (moveDown point)
moveLeftUp point = moveLeft (moveUp point)

stringToListOfListsOfChars :: String -> [[Char]]
stringToListOfListsOfChars text = map read (splitOn "\n" text)