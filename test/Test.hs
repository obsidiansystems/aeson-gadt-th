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

    it "should generate an expected ToJSON instance with options" $ do
      toJSON (Spam'Eggs 'a') `shouldBe` [aesonQQ| ["Eggs", "a"] |]
      toJSON (Spam'Life 1.2) `shouldBe` [aesonQQ| ["Life", 1.2] |]
    it "should generate an expected FromJSON Some instance with options" $ do
      fromJSON [aesonQQ| ["Eggs", "a"] |]
        `shouldMatchPattern_` (\case Success (This (Spam'Eggs 'a')) -> ())
      fromJSON [aesonQQ| ["Life", 1.2] |]
        `shouldMatchPattern_` (\case Success (This (Spam'Life 1.2)) -> ())

data Foo a where
  Bar :: Char -> Foo Char
  Baz :: Float -> Foo Float

deriving instance Show (Foo a)
deriving instance Eq (Foo a)

instance GShow Foo where gshowsPrec = showsPrec

data Spam a where
  Spam'Eggs :: Char -> Spam Char
  Spam'Life :: Float -> Spam Float

deriving instance Show (Spam a)
deriving instance Eq (Spam a)

instance GShow Spam where gshowsPrec = showsPrec

deriveJSONGADT ''Foo

deriveJSONGADTWithOptions
  (JSONGADTOptions { gadtConstructorModifier = drop 5 })
  ''Spam
