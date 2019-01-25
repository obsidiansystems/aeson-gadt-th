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
  tyVars <- replicateM arity (newName "topvar")
  let n' = foldr (\v c -> AppT c (VarT v)) (ConT n) tyVars
  (matches, typs) <- runWriterT (mapM (fmap pure . conMatchesToJSON tyVars) cons)
  let nubbedTypes = map head . group . sort $ typs -- This 'head' is safe because 'group' returns a list of non-empty lists
      constraints = map (AppT (ConT ''ToJSON)) nubbedTypes
  impl <- funD (mkName "toJSON")
    [ clause [] (normalB $ lamCaseE matches) []
    ]
  return [ InstanceD Nothing constraints (AppT (ConT ''ToJSON) n') [impl] ]
  
-- | Implementation of 'toJSON'
conMatchesToJSON :: [Name] -> Con -> WriterT [Type] Q Match
conMatchesToJSON topVars c = do
  let name = conName c
      base = nameBase name
      toJSONExp e = [| toJSON $(e) |]
  vars <- lift $ replicateM (conArity c) (newName "x")
  let body = toJSONExp $ tupE [ [| base :: String |] , tupE $ map (toJSONExp . varE) vars ]
  _ <- conMatches topVars c
  lift $ match (conP name (map varP vars)) (normalB body) []

deriveFromJSONGADT :: Name -> DecsQ
deriveFromJSONGADT n = do
  x <- reify n
  let cons = case x of
       TyConI d -> decCons d
       _ -> error "undefined"
  let wild = match wildP (normalB [|fail "deriveFromJSONGADT: Supposedly-complete GADT pattern match fell through in generated code. This shouldn't happen."|]) []
  arity <- tyConArity n
  tyVars <- replicateM (arity - 1) (newName "topvar")
  let n' = foldr (\v c -> AppT c (VarT v)) (ConT n) tyVars
  (matches, typs) <- runWriterT $ mapM (conMatchesParseJSON tyVars [|_v'|]) cons
  let nubbedTypes = map head . group . sort $ typs -- This 'head' is safe because 'group' returns a list of non-empty lists
      constraints = map (AppT (ConT ''FromJSON)) nubbedTypes
  v <- newName "v"
  parser <- funD (mkName "parseJSON")
    [ clause [varP v] (normalB [e| 
        do (tag', _v') <- parseJSON $(varE v)
           $(caseE [|tag' :: String|] $ map pure matches ++ [wild])
      |]) []
    ]
  return [ InstanceD Nothing constraints (AppT (ConT ''FromJSON) (AppT (ConT ''Some) n')) [parser] ]

substVarsWith
  :: [Name] -- Names of variables used in the instance head in argument order
  -> Type -- Result type of constructor
  -> Type -- Type of argument to the constructor
  -> Type -- Type of argument with variables substituted for instance head variables.
substVarsWith topVars (AppT resType _) = subst
  where
    topVars' = reverse topVars
    subst = \case
      AppT f x -> AppT (subst f) (subst x)
      SigT t k -> SigT (subst t) k
      VarT v -> VarT (findVar v topVars' resType)
      InfixT t1 x t2 -> InfixT (subst t1) x (subst t2)
      UInfixT t1 x t2 -> UInfixT (subst t1) x (subst t2)
      ParensT t -> ParensT (subst t)
      ConT n -> ConT n
      x -> error $ "substVarsWith: Unhandled substitution case: " <> show x

    findVar v (tv:_) (AppT _ (VarT v')) | v == v' = tv
    findVar v (_:tvs) (AppT t (VarT _)) = findVar v tvs t
    findVar v _ _ = error $ "substVarsWith: couldn't look up variable substitution for " <> show v

substVarsWith _ typ = error $ "substVarsWith: Unhandled result type: " <> show typ

conMatches
  :: [Name] -- Names of variables used in the instance head in argument order
  -> Con
  -> WriterT [Type] Q (Pat, Exp)
conMatches topVars c = do
  let name = conName c
      forTypes types resultType = do
        vars <- forM types $ \typ -> do
          x <- lift $ newName "x"
          case typ of
            AppT (ConT tn) (VarT _) -> do
              -- This may be a nested GADT, so check for special FromJSON instance
              idec <- lift $ reifyInstances ''FromJSON [AppT (ConT ''Some) (ConT tn)]
              case idec of
                [] -> do
                  tell [substVarsWith topVars resultType typ]
                  return (VarP x, VarE x)
                _ -> return $ (ConP 'This [VarP x], VarE x) -- If a FromJSON instance is found for Some f, then we use it.
            _ -> do
              tell [substVarsWith topVars resultType typ]
              return (VarP x, VarE x)
        let pat = TupP (map fst vars)
            conApp = foldl AppE (ConE name) (map snd vars)
        return (pat, conApp)
  case c of
    ForallC _ _ c' -> conMatches topVars c'
    GadtC _ tys t -> forTypes (map snd tys) t
    --NormalC _ tys -> forTypes (map snd tys) -- nb: If this comes up in a GADT-style declaration, please open an issue on the github repo with an example.
    _ -> error "conMatches: Unmatched constructor type"

conMatchesParseJSON :: [Name] -> ExpQ -> Con -> WriterT [Type] Q Match
conMatchesParseJSON topVars e c = do
  (pat, conApp) <- conMatches topVars c
  let match' = match (litP (StringL (nameBase (conName c))))
      body = doE [ bindS (return pat) [| parseJSON $e |]
                 , noBindS [| return (This $(return conApp)) |]
                 ]
  lift $ match' (normalB body) []

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
