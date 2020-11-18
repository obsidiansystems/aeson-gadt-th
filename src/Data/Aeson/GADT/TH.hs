{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Some (Some (..))
import Language.Haskell.TH hiding (cxt)
import Language.Haskell.TH.Extras (nameOfBinder)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Haskell.TH.Datatype

import System.IO (hFlush, stdout)

#if MIN_VERSION_dependent_sum(0,5,0)
#else
pattern Some :: tag a -> Some tag
pattern Some x = This x
#endif

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
  (matches, constraints') <- runWriterT (mapM (fmap pure . conMatchesToJSON opts topVars) cons)
  m <- sequence matches
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
  topVars <- makeTopVars n
  let n' = foldl (\c v -> AppT c (VarT v)) (ConT n) $ init topVars
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

splitTopVars :: [Name] -> (Set Name, Name)
splitTopVars allTopVars = case reverse allTopVars of
  (x:xs) -> (Set.fromList xs, x)
  _ -> error "splitTopVars: Empty set of variables"

-- | Implementation of 'toJSON'
conMatchesToJSON :: JSONGADTOptions -> [Name] -> ConstructorInfo -> WriterT [Type] Q Match
conMatchesToJSON opts allTopVars c = do
  let (topVars, lastVar) = splitTopVars allTopVars
      name = constructorName c
      base = gadtConstructorModifier opts $ nameBase name
      toJSONExp e = [| toJSON $(e) |]
  vars <- lift . forM (constructorFields c) $ \_ -> newName "x"
  let body = toJSONExp $ tupE [ [| base :: String |] , tupE $ map (toJSONExp . varE) vars ]
  _ <- conMatches ''ToJSON topVars lastVar c
  lift $ match (conP name (map varP vars)) (normalB body) []

-- | Implementation of 'parseJSON'
conMatchesParseJSON :: JSONGADTOptions -> [Name] -> ExpQ -> ConstructorInfo -> WriterT [Type] Q Match
conMatchesParseJSON opts allTopVars e c = do
  let (topVars, lastVar) = splitTopVars allTopVars
  (pat, conApp) <- conMatches ''FromJSON topVars lastVar c
  let match' = match (litP (StringL (gadtConstructorModifier opts $ nameBase (constructorName c))))
      body = doE [ bindS (return pat) [| parseJSON $e |]
                 , noBindS [| return (Some $(return conApp)) |]
                 ]
  lift $ match' (normalB body) []

conMatches
  :: Name -- ^ Name of class (''ToJSON or ''FromJSON)
  -> Set Name -- Names of variables used in the instance head in argument order
  -> Name -- Final type variable (the index type)
  -> ConstructorInfo
  -> WriterT [Type] Q (Pat, Exp)
conMatches clsName topVars ixVar c = do
  let mkConstraint = AppT (ConT clsName)
      name = constructorName c
      types = constructorFields c
      (constraints, equalities') = flip partition (constructorContext c) $ \case
        AppT (AppT EqualityT _) _ -> False
        _ -> True
      equalities = concat [ [(a, b), (b, a)] | AppT (AppT EqualityT a) b <- equalities' ]
  unifiedEqualities :: [Map Name Type] <- lift $ forM equalities $ \(a, b) -> unifyTypes [a, b]
  let rigidImplications :: Map Name (Set Name)
      rigidImplications = Map.unionsWith Set.union $ fmap freeTypeVariables <$> unifiedEqualities
  let expandRigids :: Set Name -> Set Name
      expandRigids rigids = Set.union rigids $ Set.unions $ Map.elems $
        restrictKeys rigidImplications rigids
      expandRigidsFully rigids =
        let rigids' = expandRigids rigids
        in if rigids' == rigids then rigids else expandRigidsFully rigids'
      rigidVars = expandRigidsFully topVars
      ixSpecialization :: Map Name Type
      ixSpecialization = restrictKeys (Map.unions unifiedEqualities) $ Set.singleton ixVar
      -- We filter out constraints which don't mention variables from the instance head mostly to avoid warnings,
      -- but a good deal more of these occur than one might expect due to the normalisation done by reifyDatatype.
      tellCxt cs = do
        tell [c | c <- applySubstitution ixSpecialization cs ]
  tellCxt constraints
  vars <- forM types $ \typ -> do
    x <- lift $ newName "x"
    let demandInstanceIfNecessary = do
          insts <- lift $ reifyInstancesWithRigids rigidVars clsName [typ]
          case insts of
            [] -> tellCxt [AppT (ConT clsName) typ]
            [InstanceD _ cxt (AppT _className ityp) _] -> do
              sub <- lift $ unifyTypes [ityp, typ]
              tellCxt $ applySubstitution sub cxt

            _ -> error $ "The following instances of " ++ show clsName ++ " for " ++ show typ ++ " exist (rigids: " ++ unwords (map show $ Set.toList rigidVars) ++ "), and I don't know which to pick:\n" ++ unlines (map (show . ppr) insts)
    case typ of
      AppT tn (VarT _) -> do
        -- This may be a nested GADT, so check for special FromJSON instance
        insts <- lift $ reifyInstancesWithRigids rigidVars clsName [AppT (ConT ''Some) tn]
        case insts of
          [] -> do
            -- No special instance, try to check the type for a straightforward one, since if we don't have one, we need to demand it.
            demandInstanceIfNecessary
            return (VarP x, VarE x)
          [InstanceD _ cxt (AppT _className (AppT (ConT _some) ityp)) _] -> do
            sub <- lift $ unifyTypes [ityp, tn]
            tellCxt $ applySubstitution sub cxt
            return (ConP 'Some [VarP x], VarE x)
          _ -> error $ "The following instances of " ++ show clsName ++ " for " ++ show (ppr [AppT (ConT ''Some) tn]) ++ " exist (rigids: " ++ unwords (map show $ Set.toList rigidVars) ++ "), and I don't know which to pick:\n" ++ unlines (map (show . ppr) insts)
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

-- | Determine the arity of a kind.
kindArity :: Kind -> Int
kindArity = \case
  ForallT _ _ t -> kindArity t
  AppT (AppT ArrowT _) t -> 1 + kindArity t
  SigT t _ -> kindArity t
  ParensT t -> kindArity t
  _ -> 0

-- | Given the name of a type constructor, determine a list of type variables bound as parameters by
-- its declaration, and the arity of the kind of type being defined (i.e. how many more arguments would
-- need to be supplied in addition to the bound parameters in order to obtain an ordinary type of kind *).
-- If the supplied 'Name' is anything other than a data or newtype, produces an error.
tyConArity' :: Name -> Q ([TyVarBndr], Int)
tyConArity' n = reify n >>= return . \case
  TyConI (DataD _ _ ts mk _ _) -> (ts, fromMaybe 0 (fmap kindArity mk))
  TyConI (NewtypeD _ _ ts mk _ _) -> (ts, fromMaybe 0 (fmap kindArity mk))
  _ -> error $ "tyConArity': Supplied name reified to something other than a data declaration: " ++ show n


-----------------------------------------------------------------------------------------------------

restrictKeys :: Ord k => Map k v -> Set k -> Map k v
restrictKeys m s =
#if MIN_VERSION_containers(0,5,8)
  Map.restrictKeys m s
#else
  Map.intersection m $ Map.fromSet (const ()) s
#endif
