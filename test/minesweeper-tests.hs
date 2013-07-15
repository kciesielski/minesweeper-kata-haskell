module Main where

import Minesweeper.Core
import Test.QuickCheck
import Test.QuickCheck()
import Test.HUnit()
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit()
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

prop_add a b = (add newEmpty Point { x = a, y = b }) Point { x = a, y = b } == True
prop_empty a b = newEmpty Point { x = a, y = b} == False
prop_union_with_empty :: MineSet -> Point -> Property
prop_union_with_empty set point = 
	contains set point ==>
	  contains (union newEmpty set) point == True
prop_union_contains_from_A :: Point -> MineSet -> MineSet -> Property
prop_union_contains_from_A point setA setB = 
	contains setA point ==> 
		contains (union setA setB) point == True
prop_union_contains_from_B :: Point -> MineSet -> MineSet -> Property
prop_union_contains_from_B point setA setB = 
	contains setB point ==> 
		contains (union setA setB) point == True

instance Arbitrary Point where 
  arbitrary = do
      vx <- choose (-1000, 1000) :: Gen Int
      vy <- choose (-1000, 1000) :: Gen Int
      return Point { x = vx, y = vy }

instance CoArbitrary Point where
	coarbitrary p = variant 1 -- I have no idea what this stuff will produce ....


tests :: [TF.Test]
tests = [
        testGroup "QuickCheck Minesweeper" [
                testProperty "add To Set"           prop_add,
                testProperty "empty set"			prop_empty,
                testProperty "set union with empty set gives the same set" prop_union_with_empty,
                testProperty "union contains point from its 1st coponent" prop_union_contains_from_A,
                testProperty "union contains point from its 2nd coponent" prop_union_contains_from_B
                ]
       ]
