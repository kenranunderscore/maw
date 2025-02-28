module Main (main) where

import Maw.CommandSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
    Maw.CommandSpec.spec
