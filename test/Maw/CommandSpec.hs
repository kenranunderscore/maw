{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Maw.CommandSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (..))

import Maw.Command

deriving via GenericArbitrary Command instance Arbitrary Command

spec :: Spec
spec = describe "encoding and decoding commands" $ do
    prop "works" $ \cmd ->
        let ByteMessage bytes = encode cmd
         in decode (fmap fromIntegral bytes) `shouldBe` Just cmd
