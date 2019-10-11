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

module Data.Aeson.GADT.TH
  ( deriveJSONGADT
  , deriveToJSONGADT
  , deriveFromJSONGADT

  , deriveJSONGADTWithOptions
  , deriveToJSONGADTWithOptions
  , deriveFromJSONGADTWithOptions

  , JSONGADTOptions(JSONGADTOptions, gadtConstructorModifier)
  , defaultJSONGADTOptions

  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Aeson
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Some (Some (..))
import Language.Haskell.TH hiding (cxt)
import Language.Haskell.TH.Extras
import Data.Set (Set)
import qualified Data.Set as Set

newtype JSONGADTOptions = JSONGADTOptions
  { gadtConstructorModifier :: String -> String }

defaultJSONGADTOptions :: JSONGADTOptions
defaultJSONGADTOptions = JSONGADTOptions
  { gadtConstructorModifier = id }

-- | Derive 'ToJSON' and 'FromJSON' instances for the named GADT
deriveJSONGADT :: Name -> DecsQ
deriveJSONGADT = deriveJSONGADTWithOptions defaultJSONGADTOptions

deriveJSONGADTWithOptions :: JSONGADTOptions -> Name -> DecsQ
deriveJSONGADTWithOptions opts n = do
  tj <- deriveToJSONGADTWithOptions opts n
  fj <- deriveFromJSONGADTWithOptions opts n
  return (tj ++ fj)

deriveToJSONGADT :: Name -> DecsQ
deriveToJSONGADT = deriveToJSONGADTWithOptions defaultJSONGADTOptions

deriveToJSONGADTWithOptions :: JSONGADTOptions -> Name -> DecsQ
deriveToJSONGADTWithOptions opts n = do
  x <- reify n
  let cons = case x of
       TyConI d -> decCons d
       _ -> error $ "deriveToJSONGADT: Name `" ++ show n ++ "' does not appear to be the name of a type constructor."
  topVars <- makeTopVars n
  let n' = foldl (\c v -> AppT c (VarT v)) (ConT n) topVars
  (matches, constraints') <- runWriterT (mapM (fmap pure . conMatchesToJSON opts (init topVars)) cons)
  let constraints = map head . group . sort $ constraints' -- This 'head' is safe because 'group' returns a list of non-empty lists
  impl <- funD (mkName "toJSON")
    [ clause [] (normalB $ lamCaseE matches) []
    ]
  return [ InstanceD Nothing constraints (AppT (ConT ''ToJSON) n') [impl] ]

makeTopVars :: Name -> Q [Name]
makeTopVars tyConName = do
  (tyVarBndrs, kArity) <- tyConArity' tyConName
  extraVars <- replicateM kArity (newName "topvar")
  return (map tyVarBndrName tyVarBndrs ++ extraVars)

deriveFromJSONGADT :: Name -> DecsQ
deriveFromJSONGADT = deriveFromJSONGADTWithOptions defaultJSONGADTOptions

deriveFromJSONGADTWithOptions :: JSONGADTOptions -> Name -> DecsQ
deriveFromJSONGADTWithOptions opts n = do
  x <- reify n
  let decl = case x of
        TyConI d -> d
        _ -> error $ "deriveFromJSONGADT: Name `" ++ show n ++ "' does not appear to be the name of a type constructor."
      cons = decCons decl
      allConNames =
        intercalate ", " $
          map (gadtConstructorModifier opts . nameBase . conName) cons
  wildName <- newName "s"
  let wild = match (varP wildName) (normalB [e|
        fail $
          "Expected tag to be one of [" <> allConNames <> "] but got: "
          <> $(varE wildName)
        |]) []
  topVars <- init <$> makeTopVars n
  let n' = foldl (\c v -> AppT c (VarT v)) (ConT n) topVars
  (matches, constraints') <- runWriterT $ mapM (conMatchesParseJSON opts topVars [|_v'|]) cons
  let constraints = map head . group . sort $ constraints' -- This 'head' is safe because 'group' returns a list of non-empty lists
  v <- newName "v"
  parser <- funD (mkName "parseJSON")
    [ clause [varP v] (normalB [e| 
        do (tag', _v') <- parseJSON $(varE v)
           $(caseE [|tag' :: String|] $ map pure matches ++ [wild])
      |]) []
    ]
  return [ InstanceD Nothing constraints (AppT (ConT ''FromJSON) (AppT (ConT ''Some) n')) [parser] ]

-- | Implementation of 'toJSON'
conMatchesToJSON :: JSONGADTOptions -> [Name] -> Con -> WriterT [Type] Q Match
conMatchesToJSON opts topVars c = do
  let name = conName c
      base = gadtConstructorModifier opts $ nameBase name
      toJSONExp e = [| toJSON $(e) |]
  vars <- lift $ replicateM (conArity c) (newName "x")
  let body = toJSONExp $ tupE [ [| base :: String |] , tupE $ map (toJSONExp . varE) vars ]
  _ <- conMatches (AppT (ConT ''ToJSON)) topVars c
  lift $ match (conP name (map varP vars)) (normalB body) []

-- | Implementation of 'parseJSON'
conMatchesParseJSON :: JSONGADTOptions -> [Name] -> ExpQ -> Con -> WriterT [Type] Q Match
conMatchesParseJSON opts topVars e c = do
  (pat, conApp) <- conMatches (AppT (ConT ''FromJSON))  topVars c
  let match' = match (litP (StringL (gadtConstructorModifier opts $ nameBase (conName c))))
      body = doE [ bindS (return pat) [| parseJSON $e |]
                 , noBindS [| return (This $(return conApp)) |]
                 ]
  lift $ match' (normalB body) []

conMatches
  :: (Type -> Type) -- ^ Function to apply to form instance constraints
  -> [Name] -- Names of variables used in the instance head in argument order
  -> Con
  -> WriterT [Type] Q (Pat, Exp)
conMatches mkConstraint topVars c = do
  let name = conName c
      tellCxt cs = do
        when (not (null (Set.intersection (foldMap freeTypeVariables cs) (Set.fromList topVars)))) $
          tell cs
      forTypes types resultType = do
        vars <- forM types $ \typ -> do
          x <- lift $ newName "x"
          case typ of
            AppT (ConT tn) (VarT _) -> do
              -- This may be a nested GADT, so check for special FromJSON instance
              idec <- lift $ reifyInstances ''FromJSON [AppT (ConT ''Some) (ConT tn)]
              case idec of
                [] -> do
                  tellCxt [mkConstraint (substVarsWith topVars resultType typ)]
                  return (VarP x, VarE x)
                _ -> return $ (ConP 'This [VarP x], VarE x) -- If a FromJSON instance is found for Some f, then we use it.
            _ -> do
              tellCxt [mkConstraint (substVarsWith topVars resultType typ)]
              return (VarP x, VarE x)
        let pat = TupP (map fst vars)
            conApp = foldl AppE (ConE name) (map snd vars)
        return (pat, conApp)
  case c of
    ForallC _ cxt (GadtC _ tys t) -> do
      tellCxt (map (substVarsWith topVars t) cxt)
      forTypes (map snd tys) t
    GadtC _ tys t -> forTypes (map snd tys) t
    --NormalC _ tys -> forTypes (map snd tys) -- nb: If this comes up in a GADT-style declaration, please open an issue on the github repo with an example.
    _ -> error "conMatches: Unmatched constructor type"

-----------------------------------------------------------------------------------------------------

-- | Determine the type variables which occur freely in a type.
freeTypeVariables :: Type -> Set Name
freeTypeVariables t = case t of
  ForallT bndrs _ t' -> Set.difference (freeTypeVariables t') (Set.fromList (map nameOfBinder bndrs))
  AppT t1 t2 -> Set.union (freeTypeVariables t1) (freeTypeVariables t2)
  SigT t _ -> freeTypeVariables t
  VarT n -> Set.singleton n
  _ -> Set.empty

-- | Determine the 'Name' being bound by a 'TyVarBndr'.
tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName = \case
  PlainTV n -> n
  KindedTV n _ -> n

-- | Determines the name of a data constructor. It's an error if the 'Con' binds more than one name (which
-- happens in the case where you use GADT syntax, and give multiple data constructor names separated by commas
-- in a type signature in the where clause).
conName :: Con -> Name
conName c = case c of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c' -> conName c'
  GadtC [n] _ _ -> n
  RecGadtC [n] _ _ -> n
  _ -> error "conName: GADT constructors with multiple names not yet supported"