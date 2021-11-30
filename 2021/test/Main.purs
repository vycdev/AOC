module Test.Main where

import Prelude
import Day1 (solution1)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "Day 1" do
      test "Solution 1"
        $ Assert.equal 1 solution1
