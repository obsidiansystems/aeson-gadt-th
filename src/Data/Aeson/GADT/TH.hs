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
import Data.Set (Set)
import Data.Some (Some (..))
import Language.Haskell.TH

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
      forTypes types resultType = do
        vars <- forM types $ \typ -> do
          x <- lift $ newName "x"
          case typ of
            AppT (ConT tn) (VarT _) -> do
              -- This may be a nested GADT, so check for special FromJSON instance
              idec <- lift $ reifyInstances ''FromJSON [AppT (ConT ''Some) (ConT tn)]
              case idec of
                [] -> do
                  tell [mkConstraint (substVarsWith topVars resultType typ)]
                  return (VarP x, VarE x)
                _ -> return $ (ConP 'This [VarP x], VarE x) -- If a FromJSON instance is found for Some f, then we use it.
            _ -> do
              tell [mkConstraint (substVarsWith topVars resultType typ)]
              return (VarP x, VarE x)
        let pat = TupP (map fst vars)
            conApp = foldl AppE (ConE name) (map snd vars)
        return (pat, conApp)
  case c of
    ForallC _ cxt (GadtC _ tys t) -> do
      tell (map (substVarsWith topVars t) cxt)
      forTypes (map snd tys) t
    GadtC _ tys t -> forTypes (map snd tys) t
    --NormalC _ tys -> forTypes (map snd tys) -- nb: If this comes up in a GADT-style declaration, please open an issue on the github repo with an example.
    _ -> error "conMatches: Unmatched constructor type"

-----------------------------------------------------------------------------------------------------

-- | Assuming that we're building an instance of the form C (T v_1 ... v_(n-1)) for some GADT T, this function
-- takes a list of the variables v_1 ... v_(n-1) used in the instance head, as well as the result type of some data
-- constructor, say T x_1 ... x_(n-1) x_n, as well as the type t of some argument to it, and substitutes any of
-- x_i (1 <= i <= n-1) occurring in t for the corresponding v_i, taking care to avoid name capture by foralls in t.
substVarsWith
  :: [Name] -- Names of variables used in the instance head in argument order
  -> Type -- Result type of constructor
  -> Type -- Type of argument to the constructor
  -> Type -- Type of argument with variables substituted for instance head variables.
substVarsWith topVars resultType argType = subst Set.empty argType
  where
    topVars' = reverse topVars
    AppT resultType' indexType = resultType
    subst bs = \case
      ForallT bndrs cxt t ->
        let bs' = Set.union bs (Set.fromList (map tyVarBndrName bndrs))
        in ForallT bndrs (map (subst bs') cxt) (subst bs' t)
      AppT f x -> AppT (subst bs f) (subst bs x)
      SigT t k -> SigT (subst bs t) k
      VarT v -> if Set.member v bs
        then VarT v
        else VarT (findVar v topVars' resultType')
      InfixT t1 x t2 -> InfixT (subst bs t1) x (subst bs t2)
      UInfixT t1 x t2 -> UInfixT (subst bs t1) x (subst bs t2)
      ParensT t -> ParensT (subst bs t)
      -- The following cases could all be covered by an "x -> x" case, but I'd rather know if new cases
      -- need to be handled specially in future versions of Template Haskell.
      PromotedT n -> PromotedT n
      ConT n -> ConT n
      TupleT k -> TupleT k
      UnboxedTupleT k -> UnboxedTupleT k
      UnboxedSumT k -> UnboxedSumT k
      ArrowT -> ArrowT
      EqualityT -> EqualityT
      ListT -> ListT
      PromotedTupleT k -> PromotedTupleT k
      PromotedNilT -> PromotedNilT
      PromotedConsT -> PromotedConsT
      StarT -> StarT
      ConstraintT -> ConstraintT
      LitT l -> LitT l
      WildCardT -> WildCardT
    findVar v _ _ | VarT v == indexType = v
    findVar v (tv:_) (AppT _ (VarT v')) | v == v' = tv
    findVar v (_:tvs) (AppT t (VarT _)) = findVar v tvs t
    findVar v _ _ = error $ "substVarsWith: couldn't look up variable substitution for " <> show v
      <> " with topVars: " <> show topVars <> " resultType: " <> show resultType <> " argType: " <> show argType

-- | Determine the 'Name' being bound by a 'TyVarBndr'.
tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName = \case
  PlainTV n -> n
  KindedTV n _ -> n

-- | Determine the arity of a kind.
kindArity :: Kind -> Int
kindArity = \case
  ForallT _ _ t -> kindArity t
  AppT (AppT ArrowT _) t -> 1 + kindArity t
  SigT t _ -> kindArity t
  ParensT t -> kindArity t
  _ -> 0

-- | Given the name of a type constructor, determine its full arity
tyConArity :: Name -> Q Int
tyConArity n = do
  (ts, ka) <- tyConArity' n
  return (length ts + ka)

-- | Given the name of a type constructor, determine a list of type variables bound as parameters by
-- its declaration, and the arity of the kind of type being defined (i.e. how many more arguments would
-- need to be supplied in addition to the bound parameters in order to obtain an ordinary type of kind *).
-- If the supplied 'Name' is anything other than a data or newtype, produces an error.
tyConArity' :: Name -> Q ([TyVarBndr], Int)
tyConArity' n = reify n >>= return . \case
  TyConI (DataD _ _ ts mk _ _) -> (ts, fromMaybe 0 (fmap kindArity mk))
  TyConI (NewtypeD _ _ ts mk _ _) -> (ts, fromMaybe 0 (fmap kindArity mk))
  _ -> error $ "tyConArity': Supplied name reified to something other than a data declaration: " <> show n

-- | Determine the constructors bound by a data or newtype declaration. Errors out if supplied with another
-- sort of declaration.
decCons :: Dec -> [Con]
decCons = \case
  DataD _ _ _ _ cs _ -> cs
  NewtypeD _ _ _ _ c _ -> [c]
  _ -> error "decCons: Declaration found was not a data or newtype declaration."

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

-- | Determine the arity of a data constructor.
conArity :: Con -> Int
conArity c = case c of
  NormalC _ ts -> length ts
  RecC _ ts -> length ts
  InfixC _ _ _ -> 2
  ForallC _ _ c' -> conArity c'
  GadtC _ ts _ -> length ts
  RecGadtC _ ts _ -> length ts
