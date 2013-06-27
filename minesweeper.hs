type Point = (Int, Int)
type MineSet = Point -> Bool

union :: MineSet -> MineSet -> MineSet
empty :: MineSet
empty = \_ -> False
add :: MineSet -> Point -> MineSet
add mineset1 point = \point2 -> (point2 == point) || mineset1 point2
singleton :: Point -> MineSet
singleton point = \point2 -> (point2 == point)
union mined1 mined2 = \point -> mined1 point || mined2 point


main = putStrLn "hello, world" 