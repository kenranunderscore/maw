module Main (main) where

import Test.Hspec

import Maw.CommandSpec qualified

main :: IO ()
main = hspec $ do
    Maw.CommandSpec.spec
