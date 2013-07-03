module Main where

import Minesweeper.Core
import Test.QuickCheck()
import Test.HUnit()
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit()
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

prop_add :: Int -> Int -> Bool
prop_add a b = (add newEmpty Point { x = a, y = b }) Point { x = a, y = b } == True

tests :: [TF.Test]
tests = [
        testGroup "QuickCheck Minesweeper" [
                testProperty "add To Set"           prop_add
                ]
       ]
