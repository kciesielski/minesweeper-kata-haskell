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

-- -------------------- test "framework" ;) ----------------------
assertTrue :: (Monad m) => Bool -> m ()
assertTrue correct = if not correct then fail "FAIL!" else return()
assertFalse :: (Monad m) => Bool -> m ()
assertFalse condition = assertTrue (not condition)
type TestCase = IO ()
-- -------------------- tests ------------------------------------
it_should_correctly_add_a_tile_to_the_set :: TestCase
it_should_correctly_add_a_tile_to_the_set = do	
	-- given
	point <- return (Point {x = 1, y = 1})
	-- when
	gameTiles <- return (addTile emptyTileSet point 2)
	-- then
	assertTrue (gameTiles point == 2)
-- -------------- suite execution --------------------------------
main :: IO()
main = do
	putStrLn("Starting test suite")
	it_should_correctly_add_a_tile_to_the_set
	putStrLn("All tests passed succesfully!")