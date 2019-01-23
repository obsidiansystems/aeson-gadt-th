{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.GADT.TH (deriveJSONGADT, deriveToJSONGADT, deriveFromJSONGADT) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Aeson
import Data.Dependent.Sum
import Data.Functor.Classes
import Data.List
import Data.Maybe
import Data.Some (Some (..))
import Language.Haskell.TH

-- | Derive 'ToJSON' and 'FromJSON' instances for the named GADT
deriveJSONGADT :: Name -> DecsQ
deriveJSONGADT n = do
  tj <- deriveToJSONGADT n
  fj <- deriveFromJSONGADT n
  return (tj ++ fj)

decCons :: Dec -> [Con]
decCons = \case
  DataD _ _ _ _ cs _ -> cs
  NewtypeD _ _ _ _ c _ -> [c]
  _ -> error "undefined"

conName :: Con -> Name
conName c = case c of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c' -> conName c'
  GadtC [n] _ _ -> n
  RecGadtC [n] _ _ -> n
  _ -> error "conName: GADT constructors with multiple names not yet supported"

conArity :: Con -> Int
conArity c = case c of
  NormalC _ ts -> length ts
  RecC _ ts -> length ts
  InfixC _ _ _ -> 2
  ForallC _ _ c' -> conArity c'
  GadtC _ ts _ -> length ts
  RecGadtC _ ts _ -> length ts

deriveToJSONGADT :: Name -> DecsQ
deriveToJSONGADT n = do
  x <- reify n
  let cons = case x of
       TyConI d -> decCons d
       _ -> error "undefined"
  arity <- tyConArity n
  tyVars <- replicateM arity (newName "a")
  let n' = foldr (\v x -> AppT x (VarT v)) (ConT n) tyVars
  [d|
    instance ToJSON $(pure n') where
      toJSON r = $(caseE [|r|] $ map conMatchesToJSON cons)
    |]

{-
  tyVars <- replicateM (arity - 1) (newName "a")
  let n' = foldr (\v x -> AppT x (VarT v)) (ConT n) tyVars
  [d| instance ArgDict $(pure n') where
        type ConstraintsFor  $(pure n') $(varT c) = $(pure constraints)
        type ConstraintsFor' $(pure n') $(varT c) $(varT g) = $(pure constraints')
        argDict = $(LamCaseE <$> matches n 'argDict)
        argDict' = $(LamCaseE <$> matches n 'argDict')
|]
-}


deriveFromJSONGADT :: Name -> DecsQ
deriveFromJSONGADT n = do
  x <- reify n
  let cons = case x of
       TyConI d -> decCons d
       _ -> error "undefined"
  let wild = match wildP (normalB [|fail "deriveFromJSONGADT: Supposedly-complete GADT pattern match fell through in generated code. This shouldn't happen."|]) []
  arity <- tyConArity n
  tyVars <- replicateM (arity - 1) (newName "a")
  let n' = foldr (\v x -> AppT x (VarT v)) (ConT n) tyVars
  (matches, typs) <- runWriterT $ mapM (conMatchesParseJSON [|v'|]) cons
  let nubbedTypes = map head . group . sort $ typs -- This 'head' is safe because 'group' returns a list of non-empty lists
      constraints = foldl AppT (TupleT (length nubbedTypes)) $
        map (AppT (ConT ''FromJSON)) nubbedTypes
  [d|
    instance $(pure constraints) => FromJSON (Some $(pure n')) where
      parseJSON v = do
        (tag', v') <- parseJSON v
        $(caseE [|tag' :: String|] $ map pure matches ++ [wild])
    |]

-- | Implementation of 'toJSON'
conMatchesToJSON :: Con -> MatchQ
conMatchesToJSON c = do
  let name = conName c
      base = nameBase name
      toJSONExp e = [| toJSON $(e) |]
  vars <- replicateM (conArity c) (newName "x")
  let body = toJSONExp $ tupE [ [| base :: String |] , tupE $ map (toJSONExp . varE) vars ]
  match (conP name (map varP vars)) (normalB body) []


-- | Implementation of 'parseJSON'
conMatchesParseJSON :: ExpQ -> Con -> WriterT [Type] Q Match
conMatchesParseJSON e c = do
  let name = conName c
      match' = match (litP (StringL (nameBase name)))
  let forTypes types = do
        vars <- forM types $ \typ -> do
          x <- lift $ newName "x"
          case typ of
            AppT (ConT tn) (VarT vn) -> do
              -- This may be a nested GADT, so check for special FromJSON instance
              idec <- lift $ reifyInstances ''FromJSON [AppT (ConT ''Some) (ConT tn)]
              case idec of
                [] -> do
                  tell [typ]
                  return (VarP x, VarE x)
                _ -> return $ (ConP 'This [VarP x], VarE x) -- If a FromJSON instance is found for Some f, then we use it.
            _ -> do
              tell [typ]
              return (VarP x, VarE x)
        let pat = return $ TupP (map fst vars)
            conApp = return $ foldl AppE (ConE name) (map snd vars)
            body = doE [ bindS pat [| parseJSON $e |]
                       , noBindS [| return (This $conApp) |]
                       ]
        lift $ match' (normalB body) []
  case c of
    ForallC _ _ c' -> conMatchesParseJSON e c'
    GadtC _ tys _ -> forTypes (map snd tys)
    NormalC _ tys -> forTypes (map snd tys)
    _ -> error "conMatchesParseJSON: Unmatched constructor type"

kindArity :: Kind -> Int
kindArity = \case
  ForallT _ _ t -> kindArity t
  AppT (AppT ArrowT _) t -> 1 + kindArity t
  SigT t _ -> kindArity t
  ParensT t -> kindArity t
  _ -> 0

tyConArity :: Name -> Q Int
tyConArity n = reify n >>= return . \case
  TyConI (DataD _ _ ts mk _ _) -> fromMaybe 0 (fmap kindArity mk) + length ts
  _ -> error $ "tyConArity: Supplied name reified to something other than a data declaration: " <> show n
