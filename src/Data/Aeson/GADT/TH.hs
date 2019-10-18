{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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
{-# LANGUAGE PolyKinds #-}


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

import Language.Haskell.TH.Datatype

import System.IO (hFlush, stdout)

-- Do not export this type family, it must remain empty. It's used as a way to trick GHC into not unifying certain type variables.
type family Skolem :: k -> k

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName = \case
  PlainTV n -> n
  KindedTV n _ -> n

skolemize :: Set Name -> Type -> Type
skolemize rigids t = case t of
  ForallT bndrs cxt t' -> ForallT bndrs cxt (skolemize (Set.difference rigids (Set.fromList (map tyVarBndrName bndrs))) t')
  AppT t1 t2 -> AppT (skolemize rigids t1) (skolemize rigids t2)
  SigT t k -> SigT (skolemize rigids t) k
  VarT v -> if Set.member v rigids
    then AppT (ConT ''Skolem) (VarT v)
    else t
  InfixT t1 n t2 -> InfixT (skolemize rigids t1) n (skolemize rigids t2)
  UInfixT t1 n t2 -> UInfixT (skolemize rigids t1) n (skolemize rigids t2)
  ParensT t -> ParensT (skolemize rigids t)
  _ -> t

reifyInstancesWithRigids :: Set Name -> Name -> [Type] -> Q [InstanceDec]
reifyInstancesWithRigids rigids cls tys = reifyInstances cls (map (skolemize rigids) tys)

-- | Determine the type variables which occur freely in a type.
freeTypeVariables :: Type -> Set Name
freeTypeVariables t = case t of
  ForallT bndrs _ t' -> Set.difference (freeTypeVariables t') (Set.fromList (map nameOfBinder bndrs))
  AppT t1 t2 -> Set.union (freeTypeVariables t1) (freeTypeVariables t2)
  SigT t _ -> freeTypeVariables t
  VarT n -> Set.singleton n
  _ -> Set.empty

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
  info <- reifyDatatype n
  let cons = datatypeCons info
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
  info <- reifyDatatype n

  let cons = datatypeCons info
      allConNames =
        intercalate ", " $
          map (gadtConstructorModifier opts . nameBase . constructorName) cons
  wildName <- newName "s"
  let wild = match (varP wildName) (normalB [e|
        fail $
          "Expected tag to be one of [" ++ allConNames ++ "] but got: "
          ++ $(varE wildName)
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
conMatchesToJSON :: JSONGADTOptions -> [Name] -> ConstructorInfo -> WriterT [Type] Q Match
conMatchesToJSON opts topVars c = do
  let name = constructorName c
      base = gadtConstructorModifier opts $ nameBase name
      toJSONExp e = [| toJSON $(e) |]
  vars <- lift . forM (constructorFields c) $ \_ -> newName "x"
  let body = toJSONExp $ tupE [ [| base :: String |] , tupE $ map (toJSONExp . varE) vars ]
  _ <- conMatches ''ToJSON topVars c
  lift $ match (conP name (map varP vars)) (normalB body) []

-- | Implementation of 'parseJSON'
conMatchesParseJSON :: JSONGADTOptions -> [Name] -> ExpQ -> ConstructorInfo -> WriterT [Type] Q Match
conMatchesParseJSON opts topVars e c = do
  (pat, conApp) <- conMatches ''FromJSON topVars c
  let match' = match (litP (StringL (gadtConstructorModifier opts $ nameBase (constructorName c))))
      body = doE [ bindS (return pat) [| parseJSON $e |]
                 , noBindS [| return (Some $(return conApp)) |]
                 ]
  lift $ match' (normalB body) []

conMatches
  :: Name -- ^ Name of class (''ToJSON or ''FromJSON)
  -> [Name] -- Names of variables used in the instance head in argument order
  -> ConstructorInfo
  -> WriterT [Type] Q (Pat, Exp)
conMatches clsName topVars c = do
  let mkConstraint = AppT (ConT clsName)
      name = constructorName c
      types = constructorFields c
      topVarSet = Set.fromList topVars
      -- We filter out constraints which don't mention variables from the instance head mostly to avoid warnings,
      -- but a good deal more of these occur than one might expect due to the normalisation done by reifyDatatype.
      tellCxt cs =
        tell [c | c <- cs, not (null (Set.intersection (freeTypeVariables c) topVarSet))]
  vars <- forM types $ \typ -> do
    x <- lift $ newName "x"
    let demandInstanceIfNecessary = do
          insts <- lift $ reifyInstancesWithRigids topVarSet clsName [typ]
          case insts of
            [] -> tellCxt [AppT (ConT clsName) typ]
            [(InstanceD _ cxt _ _)] -> tellCxt cxt
            _ -> error $ "The following instances of " ++ show clsName ++ " for " ++ show typ ++ " exist, and I don't know which to pick:\n" ++ unlines (map (show . ppr) insts)
    case typ of
      AppT (ConT tn) (VarT _) -> do
        -- This may be a nested GADT, so check for special FromJSON instance
        insts <- lift $ reifyInstancesWithRigids topVarSet clsName [AppT (ConT ''Some) (ConT tn)]
        case insts of
          [] -> do
            -- No special instance, try to check the type for a straightforward one, since if we don't have one, we need to demand it.
            demandInstanceIfNecessary
            return (VarP x, VarE x)
          [(InstanceD _ cxt _ _)] -> do
            tellCxt cxt
            return (ConP 'Some [VarP x], VarE x)
          _ -> error $ "The following instances of " ++ show clsName ++ " for " ++ show (ppr [AppT (ConT ''Some) (ConT tn)]) ++ " exist, and I don't know which to pick:\n" ++ unlines (map (show . ppr) insts)
      _ -> do
        demandInstanceIfNecessary  
        return (VarP x, VarE x)
  let pat = TupP (map fst vars)
      conApp = foldl AppE (ConE name) (map snd vars)
  return (pat, conApp)

-----------------------------------------------------------------------------------------------------

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
