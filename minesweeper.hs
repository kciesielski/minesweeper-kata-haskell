module Main where	

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

data Point = Point {x:: Int, y::Int}
type MineSet = Set Point


data WorldRectangle = WorldRectangle {
x1:: Int, y1:: Int, x2:: Int, y2:: Int } deriving (Show)

type TileSet = Point -> Int
addTile :: TileSet -> Point -> Int -> TileSet
emptyTileSet :: TileSet
emptyTileSet = \_ -> -1
addTile set point int = \p -> if (x p == x point && y p == y point) then int else set p


data World = World {bounds:: WorldRectangle, mines:: MineSet, tiles:: TileSet}

withinWorld :: WorldRectangle -> Point -> Bool

withinWorld rect = \point -> x1 rect <= x point && x2 rect >= x point && y1 rect <= y point && y2 rect >= y point

-- -------------------- tests ------------
main :: IO()
main = do
	putStrLn("Starting test suite")
	putStrLn("Testing tileset")
	putStrLn("It should correctly add a tile to the set")
	-- given
	let point = Point {x = 1, y = 1}
	-- when
	let gameTiles = addTile emptyTileSet point 12
	-- then
	let result = gameTiles point == 12
	if not result then error "FAIL!" else putStrLn("PASSED")
	putStrLn("All tests passed succesfully!")