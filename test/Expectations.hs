-- | Some useful helper expectations for use with Hspec.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Expectations where

import Control.Exception (PatternMatchFail, evaluate, throwIO, try)
import Data.Maybe
import GHC.Stack (HasCallStack, callStack, getCallStack, SrcLoc)
import Test.Hspec
import qualified Test.HUnit.Lang as HUnit

-- | Assert that a pattern match succeeds; may require -fno-warn-incomplete-patterns
expectPattern :: (HasCallStack, Show a) => (a -> b) -> a -> IO b
expectPattern f a =
  try (evaluate $ f a) >>= \case
    Right b -> pure b
    Left (e :: PatternMatchFail) ->
      throwHUnit $ "Pattern match failed, value was: " <> show a

-- | Same as 'expectPattern' but with its arguments flipped.
shouldMatchPattern :: (HasCallStack, Show a) => a -> (a -> b) -> IO b
shouldMatchPattern = flip expectPattern

-- | Same as 'shouldMatchPattern' but with the return type specialized as unit.
-- Useful for pattern matching on GADTs.
shouldMatchPattern_ :: (HasCallStack, Show a) => a -> (a -> ()) -> IO ()
shouldMatchPattern_ = shouldMatchPattern

-- | Obtain the source location given a reverse call stack index.
callStackLoc :: (HasCallStack) => Int -> Maybe SrcLoc
callStackLoc index = fmap snd $ listToMaybe $ drop index $ reverse $ getCallStack callStack

-- | Throw an test failed exception, defaulting the source location to the caller's caller.
throwHUnit :: (HasCallStack) => String -> IO a
throwHUnit = throwHUnitWithLoc 0

-- | Throw a test failure exception with source location determined by the supplied reverse call stack index.
throwHUnitWithLoc :: (HasCallStack) => Int -> String -> IO a
throwHUnitWithLoc index msg = throwIO $ HUnit.HUnitFailure (callStackLoc index) $ HUnit.Reason msg
