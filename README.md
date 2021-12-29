aeson-gadt-th
=============
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/aeson-gadt-th.svg)](https://hackage.haskell.org/package/aeson-gadt-th) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/aeson-gadt-th/badge)](https://matrix.hackage.haskell.org/#/package/aeson-gadt-th) [![Github CI](https://github.com/obsidiansystems/aeson-gadt-th/workflows/github-action/badge.svg)](https://github.com/obsidiansystems/aeson-gadt-th/actions) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/aeson-gadt-th/blob/master/LICENSE)

Provides Template Haskell expressions for deriving `ToJSON` and `FromJSON` instances for GADTs.

Example Usage:
--------------

```haskell
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# OPTIONS_GHC -ddump-splices #-}
>
> import Data.Aeson
> import Data.Aeson.GADT.TH
> import Data.Dependent.Map (DMap)
> import Data.Dependent.Sum (DSum)
> import Data.Functor.Identity
> import Data.GADT.Compare
> import Data.GADT.Show.TH
> import Data.Some (Some(..))
>
> data A :: * -> * where
>   A_a :: A a
>   A_b :: Int -> A ()
>
> deriveJSONGADT ''A
> deriveGShow ''A
>
> data B c :: * -> * where
>   B_a :: c -> A a -> B c a
>   B_x :: B c a
>
> deriveJSONGADT ''B
>
> data C t :: * -> * where
>   C_t :: t -> C t t
>
> deriveJSONGADT ''C
>
> data D t x :: * -> * where
>   D_t :: t -> D t x t
>   D_x :: x -> D t x x
>   D_i :: Int -> D t x Int
>
> deriveJSONGADT ''D
>
> data Auth token a where
>   Auth_Login :: String -> String -> Auth token (Either String token)
>
> deriveJSONGADT ''Auth
>
> -- Some real-world-ish examples.
>
> -- | Edit operations for `LabelledGraph`
> data LabelledGraphEdit v vm em :: * -> * where
>   LabelledGraphEdit_ClearAll :: LabelledGraphEdit v vm em ()
>   LabelledGraphEdit_AddVertex :: vm -> LabelledGraphEdit v vm em v
>   LabelledGraphEdit_AddEdge :: v -> v -> em -> LabelledGraphEdit v vm em ()
>   LabelledGraphEdit_SetVertexProperties :: v -> vm -> LabelledGraphEdit v vm em ()
>   LabelledGraphEdit_SetEdgeProperties :: v -> v -> em -> LabelledGraphEdit v vm em ()
>
> -- | PropertyGraphEdit operatios for `PropertyGraph`
> data PropertyGraphEdit v vp ep r where
>   PropertyGraphEdit_ClearAll :: PropertyGraphEdit v vp ep ()
>   PropertyGraphEdit_AddVertex :: (DMap vp Identity) -> PropertyGraphEdit v vp ep v
>   PropertyGraphEdit_AddEdge :: v -> v -> (DMap ep Identity) -> PropertyGraphEdit v vp ep ()
>   PropertyGraphEdit_SetVertexProperty :: GCompare vp => v -> DSum vp Identity -> PropertyGraphEdit v vp ep ()
>   PropertyGraphEdit_SetEdgeProperty :: GCompare ep => v -> v -> DSum ep Identity -> PropertyGraphEdit v vp ep ()
>
> -- | View operations for `LabelledGraph`
> data LabelledGraphView v vm em :: * -> * where
>   LabelledGraphView_All :: LabelledGraphView v vm em ()
>   LabelledGraphView_GetVertexProperties :: v -> LabelledGraphView v vm em vm
>   LabelledGraphView_GetEdgeProperties :: v -> v -> LabelledGraphView v vm em em
>
> deriveJSONGADT ''LabelledGraphEdit
> deriveJSONGADT ''PropertyGraphEdit
> deriveJSONGADT ''LabelledGraphView
>
> main :: IO ()
> main = do
>   putStrLn $ unlines
>     [ "Encoding of A_a:"
>     , show $ encode A_a
>     , "Decoding of encoded A_a:"
>     , show (decode $ encode A_a :: Maybe (Some A))
>     ]
>
>   putStrLn $ unlines
>     [ "Encoding of (A_b 1):"
>     , show $ encode (A_b 1)
>     , "Decoding of encoded (A_b 1):"
>     , show (decode $ encode (A_b 1) :: Maybe (Some A))
>     ]
>
>   putStrLn $ unlines
>     [ "Encoding of (B_a 'a' (A_b 1)):"
>     , show $ encode (B_a 'a' (A_b 1))
>     , "Decoding of encoded (B_a 'a' (A_b 1)):"
>     , case (decode $ encode (B_a 'a' (A_b 1)) :: Maybe (Some (B Char))) of
>         Just (Some (B_a 'a' (A_b 1))) -> "Succeeded"
>         _-> "Failed"
>     ]
```

***

[![Obsidian Systems](https://obsidian.systems/static/images/ObsidianSystemsLogo.svg)](https://obsidian.systems)
