{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.GADT.Show
import Data.Some
import Data.Aeson
import Data.Aeson.QQ
import Data.Aeson.GADT.TH
import Expectations
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "aeson-gadt-th" $ do
    it "should generate an expected ToJSON instance" $ do
      toJSON (Bar 'a') `shouldBe` [aesonQQ| ["Bar", "a"] |]
      toJSON (Baz 1.2) `shouldBe` [aesonQQ| ["Baz", 1.2] |]
    it "should generate an expected FromJSON Some instance" $ do
      fromJSON [aesonQQ| ["Bar", "a"] |]
        `shouldMatchPattern_` (\case Success (This (Bar 'a')) -> ())
      fromJSON [aesonQQ| ["Baz", 1.2] |]
        `shouldMatchPattern_` (\case Success (This (Baz 1.2)) -> ())

data Foo a where
  Bar :: Char -> Foo Char
  Baz :: Float -> Foo Float

deriving instance Show (Foo a)
deriving instance Eq (Foo a)

instance GShow Foo where gshowsPrec = showsPrec

deriveJSONGADT ''Foo
