# aeson-gadt-th

Provides Template Haskell expressions for deriving `ToJSON` and `FromJSON` instances for GADTs.

## Example Usage:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -ddump-splices #-}

import Data.Aeson
import Data.Aeson.GADT.TH

import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum)
import Data.Functor.Identity
import Data.GADT.Compare

data A :: * -> * where
  A_a :: A a
  A_b :: Int -> A ()

data B c :: * -> * where
  B_a :: c -> A a -> B c a
  B_x :: B c a

data C t :: * -> * where
  C_t :: t -> C t t

deriveJSONGADT ''A
deriveJSONGADT ''B
deriveJSONGADT ''C

-- Some real-world-ish examples.

-- | Edit operations for `LabelledGraph`
data LabelledGraphEdit v vm em :: * -> * where
  LabelledGraphEdit_ClearAll :: LabelledGraphEdit v vm em ()
  LabelledGraphEdit_AddVertex :: vm -> LabelledGraphEdit v vm em v
  LabelledGraphEdit_AddEdge :: v -> v -> em -> LabelledGraphEdit v vm em ()
  LabelledGraphEdit_SetVertexProperties :: v -> vm -> LabelledGraphEdit v vm em ()
  LabelledGraphEdit_SetEdgeProperties :: v -> v -> em -> LabelledGraphEdit v vm em ()

-- | PropertyGraphEdit operatios for `PropertyGraph`
data PropertyGraphEdit v (vp :: * -> *) ep r where
  PropertyGraphEdit_ClearAll :: PropertyGraphEdit v vp ep ()
  PropertyGraphEdit_AddVertex :: (DMap vp Identity) -> PropertyGraphEdit v vp ep v
  PropertyGraphEdit_AddEdge :: v -> v -> (DMap ep Identity) -> PropertyGraphEdit v vp ep ()
  PropertyGraphEdit_SetVertexProperty :: GCompare vp => v -> DSum vp Identity -> PropertyGraphEdit v vp ep ()
  PropertyGraphEdit_SetEdgeProperty :: GCompare ep => v -> v -> DSum ep Identity -> PropertyGraphEdit v vp ep ()

-- | View operations for `LabelledGraph`
data LabelledGraphView v vm em :: * -> * where
  LabelledGraphView_All :: LabelledGraphView v vm em ()
  LabelledGraphView_GetVertexProperties :: v -> LabelledGraphView v vm em vm
  LabelledGraphView_GetEdgeProperties :: v -> v -> LabelledGraphView v vm em em

deriveJSONGADT ''LabelledGraphEdit
deriveJSONGADT ''PropertyGraphEdit
deriveJSONGADT ''LabelledGraphView

main :: IO ()
main = return ()
```

## Encoding:
```
encode A_a
> "[\"A_a\",[]]"
```

## Decoding:

When decoding a JSON-encoded GADT, the result will be wrapped using [Data.Some.This](http://hackage.haskell.org/package/dependent-sum-0.4/docs/Data-Some.html).
```
case (decode $ encode A_a) :: Maybe (Some A) of
  Nothing -> error "Couldn't decode
  Just (This A_a) -> putStrLn "it worked"
  Just (This A_b) -> putStrLn "wat"
> it worked
```
