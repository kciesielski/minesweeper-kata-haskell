module Main where

import Minesweeper.Core
import Test.QuickCheck
import Test.QuickCheck()
import Test.HUnit()
import Test.HUnit
import Test.Utils
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit()
import Test.Framework.Providers.HUnit

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
prop_point_up_down :: Point -> Bool
prop_point_up_down point = 
  moveDown (moveUp point) == point
prop_point_downleft_upright :: Point -> Bool
prop_point_downleft_upright point =
  moveLeftDown (moveRightUp point) == point

instance Arbitrary Point where 
  arbitrary = do
      vx <- choose (-1000, 1000) :: Gen Int
      vy <- choose (-1000, 1000) :: Gen Int
      return Point { x = vx, y = vy }

instance CoArbitrary Point where
	coarbitrary p = variant 1 -- I have no idea what this stuff will produce ....

--
testStringToListOfListsOfChars :: (String, [[Char]]) -> Assertion
testStringToListOfListsOfChars (input, expectedOutput) = expectedOutput @=? stringToListOfListsOfChars input

dataStringToListOfListsOfChars :: [(String, [[Char]])]

dataStringToListOfListsOfChars = [
  ("test\ntest2", [['t','e','s','t'], ['t','e','s','t','2']]),
  ("test", [['t','e','s','t']])]
--
testBuildMineSetOfCharArrayShouldContainMines :: ([[Char]], Point) -> Assertion
testBuildMineSetOfCharArrayShouldContainMines (charArray, point) =  buildMineSetOfCharArray charArray point @=? True

dataCharArraysWithMinesets :: [([[Char]], Point)]
dataCharArraysWithMinesets = [
  ([['.','.','*'],['.','.','.'],['.','.','.']], Point { x = 2, y = 0}),
  ([['*','.','*'],['.','.','.'],['.','.','.']], Point { x = 2, y = 0}),
  ([['*','.','*'],['.','.','.'],['.','.','.']], Point { x = 0, y = 0}),
  ([['*','.','*'],['.','.','.'],['.','*','.']], Point { x = 1, y = 2})]
--
testBuildMineSetOfCharArrayShouldNotContainMines :: ([[Char]], Point) -> Assertion
testBuildMineSetOfCharArrayShouldNotContainMines (charArray, point) =  buildMineSetOfCharArray charArray point @=? False

dataCharArraysWithNotMinesets :: [([[Char]], Point)]
dataCharArraysWithNotMinesets = [
  ([['.','.','*'],['.','.','.'],['.','.','.']], Point { x = -1, y = 0}),
  ([['.','.','*'],['.','.','.'],['.','.','.']], Point { x = 0, y = -1}),
  ([['.','.','*'],['.','.','.'],['.','.','.']], Point { x = 1, y = 4}),
  ([['.','.','*'],['.','.','.'],['.','.','.']], Point { x = 4, y = 1}),
  ([['.','.','*'],['.','.','.'],['.','.','.']], Point { x = 1, y = 1})]
--
hintShouldReturnSumOfNeighborMines :: ([[Char]], Point, Char) -> Assertion
hintShouldReturnSumOfNeighborMines (charArray, point, expectedNumber) = hint (buildMineSetOfCharArray charArray) point @=? expectedNumber

dataHintShouldReturnSumOfNeighborMines :: [([[Char]], Point, Char)]
dataHintShouldReturnSumOfNeighborMines = [
  ([['.','.','*'],['.','.','.'],['.','.','.']], Point { x = 0, y = 0}, '0'),
  ([['.','.','*'],['.','.','.'],['.','.','.']], Point { x = 0, y = 1}, '0'),
  ([['.','.','*'],['.','.','.'],['.','.','.']], Point { x = 1, y = 0}, '1'),
  ([['.','*','*'],['.','.','.'],['*','*','.']], Point { x = 1, y = 1}, '4'),
  ([['.','*','*'],['.','.','.'],['*','*','.']], Point { x = 1, y = 0}, '*')]
--
tests :: [TF.Test]
tests = [
        testGroup "QuickCheck Minesweeper" [
                testWithProvider "string to list of lists" testStringToListOfListsOfChars dataStringToListOfListsOfChars,
                testWithProvider "built mine set contains expected points" testBuildMineSetOfCharArrayShouldContainMines dataCharArraysWithMinesets,
                testWithProvider "built mine set does not contain expected points" testBuildMineSetOfCharArrayShouldNotContainMines dataCharArraysWithNotMinesets,
                testWithProvider "hint return correct number of neighbors" hintShouldReturnSumOfNeighborMines dataHintShouldReturnSumOfNeighborMines,
                testProperty "add To Set"           prop_add,
                testProperty "empty set"			prop_empty,
                testProperty "set union with empty set gives the same set" prop_union_with_empty,
                testProperty "union contains point from its 1st coponent" prop_union_contains_from_A,
                testProperty "union contains point from its 2nd coponent" prop_union_contains_from_B,
                testProperty "move up and down returns to same point" prop_point_up_down,
                testProperty "move down-left and up-right returns to same point" prop_point_downleft_upright

                ]
       ]
