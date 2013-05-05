module TypChk where

import Prelude hiding (foldr, lookup, concat, concatMap, fail, mapM, sequence);

import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad hiding (fail, mapM, sequence);
import Control.Monad.Failure.Class;
import Control.Monad.Gen.Class;
import Data.Core;
import Data.Eq.Unicode;
import Data.Foldable;
import Data.Foldable.Unicode;
import Data.Traversable;
import Data.Maybe (fromMaybe);
import Data.Map as Map hiding (foldr);
import Data.Set as Set hiding (foldr);
import Data.RFunctor;
import Util;

type Ψ b = Map b (Expr b);
type Γ b = Map b (Δ b, Expr b); -- polymorphic type environment
type Δ b = Map b (Expr b); -- monomorphic type environment
type Σ b = [(Expr b, Expr b)]; -- type equations

(/.) :: (Ord b) => Expr b -> Ψ b -> Expr b;
Var v         /. ψ = fromMaybe (Var v) (lookup v ψ);
Λ cs          /. ψ = Λ ((\ (m, x) -> (m, x /. foldr Map.delete ψ (boundVars m))) <$> cs);
Ply f x       /. ψ = Ply (f /. ψ) (x /. ψ);
Let bs x      /. ψ = error "type-level let";
Note t x      /. ψ = Note t (x /. ψ);
Constructor c /. ψ = Constructor c;
ForAll v x    /. ψ = ForAll v (x /. Map.delete v ψ);

unify :: (Ord b, Applicative m, MonadFailure [TypChkFailure b] m) => Σ b -> m (Ψ b);
unify [] = return Map.empty;
unify ((Var u, Var v):σ) | u == v = unify σ;
unify ((Var u, y):σ)
  | u ∈ freeVars y = liftA2 Map.union (fail [InfiniteTypeFailure u y]) (unify σ)
  | otherwise = let {
      ψ = Map.singleton u y;
    } in Map.union ψ <$> fmap (/. ψ) <$> unify σ
  ;
unify ((x, Var v):σ) = unify ((Var v, x):σ);
unify ((Ply f x, Ply g y):σ) = unify ((f, g):(x, y):σ);
unify ((Constructor c1, Constructor c2):σ)
  | c1 == c2  = unify σ
  | otherwise = liftA2 Map.union (fail [TypeMismatchFailure (Constructor c1) (Constructor c2)]) (unify σ)
  ;
unify ((x, y):σ) = liftA2 Map.union (fail [TypeMismatchFailure x y]) (unify σ);

δσ :: (Ord b, Applicative m, MonadGen b m) => [Δ b] -> m (Σ b);
δσ = traverse (Map.assocs & traverse (const (Var <$> gen) *=* return)) & fmap concat;

freshen :: (Ord b, Functor m, Foldable v, MonadGen b' m) => v b -> m (Map b (Expr b'));
freshen = foldr (Map.insert & flip fmap (Var <$> gen) & ap) (return Map.empty);

-- Dr. Gergõ Érdi's algorithm, given in "Compositional Type Checking"
findTermType :: (Ord b, Applicative m, MonadFailure [TypChkFailure b] m, MonadGen b m) => Γ b -> Expr b -> m (Δ b, Expr b);
findTermType γ (Ply f x) =
  join $
  liftA3
  (\ (δf, tf) (δx, tx) ty ->
   (\ ψ -> (Map.unions $ fmap (/. ψ) <$> [δf, δx], ty /. ψ)) <$>
   ((:) (tf, tx --> ty) <$> δσ [δf, δx] >>= unify))
  (findTermType γ f) (findTermType γ x) (Var <$> gen);
findTermType γ (Λ cs) =
  traverse (findMatchType *=* findTermType γ) cs >>= \ δtδts ->
  join (liftA2 (,)) (Var <$> gen) >>= \ (α, τ0) ->
  (liftA2 ∘ liftA2) (++)
  (concatMap (sequence [fst ∘ fst, fst ∘ snd]) & δσ)
  (fmap (snd & (,) τ0 *** snd & (,) α) >>> unzip >>> uncurry (++) >>> return)
  δtδts >>= unify >>= \ ψ ->
  return (fmap (fst *** fst >>> fmap (/. ψ) *** id >>> uncurry Map.difference) & Map.unions $ δtδts, (τ0 /. ψ) --> (α /. ψ));
findTermType γ (Constructor (C _ ts t)) =
  (freshen ∘ Set.unions) (freeVars <$> t:ts) >>= \ ψ ->
  return (Map.empty, foldr (-->) (t /. ψ) ((/. ψ) <$> ts));
findTermType γ (Var v) =
  maybe ((Var >>> Map.singleton v &&& id) <$> gen) (\ (δ, t) -> fmap (\ ψ -> ((/. ψ) <$> δ, t /. ψ)) ∘ freshen ∘ Set.unions ∘ fmap freeVars $ t : Map.elems δ) (Map.lookup v γ);
findTermType γ (Let bm y) =
  Map.mapWithKey (\ v -> Map.delete v *** id) <$> traverse (findTermType γ) bm >>= \ γ0 ->
  findTermType γ0 y >>= \ (δ, t) ->
  let { δs = δ : Map.elems (fst <$> γ0); } in
  (δσ >=> unify) δs >>= \ ψ ->
  return (Map.unions $ fmap (/. ψ) <$> δs, t /. ψ);
findTermType γ (Note t x) =
  (Map.empty, t) <$ (findTermType γ x >>= pure & δσ *=* (,) t & pure & return >>= unify ∘ uncurry (++));

findMatchType :: (Ord b, Applicative m, MonadFailure [TypChkFailure b] m, MonadGen b m) => Match b -> m (Δ b, Expr b);
findMatchType (MatchStruct c@(C _ ts t) ms)
  | length ts ≠ length ms = fail [MissaturatedConstructorFailure c (length ms)]
  | otherwise =
      unzip <$> traverse findMatchType ms >>= \ (δs, ss) ->
      (++ zip ss ts) <$> δσ δs >>= unify >>= \ ψ ->
      liftA2 (,) (δσ >=> unify $ fmap (/. ψ) <$> δs) (pure $ t /. ψ);
findMatchType (MatchStruct _ ms) = fail [(const :: a -> a -> a) InalgebraicTypeFailure (TypeMismatchFailure undefined $ (\ (MatchAs v _) -> Var v) (head ms))];
findMatchType (MatchTuple ms) = unzip <$> traverse findMatchType ms >>= \ (δs, ts) ->
                                δσ δs >>= unify >>= \ ψ ->
                                liftA2 (,) (δσ >=> unify $ fmap (/. ψ) <$> δs) (pure $ Tuple ts);
findMatchType (MatchAny)      = (,) Map.empty ∘ Var <$> gen;
findMatchType (MatchAs v m)   = (\ (δ, t) -> (Map.insert v t δ, t)) <$> findMatchType m;
findMatchType (MatchLazy m)   = findMatchType m;
findMatchType (MatchNote t m) = (Map.empty, t) <$ (findMatchType m >>= pure & δσ *=* (,) t & pure & return >>= unify ∘ uncurry (++));

freeVars :: (Ord b) => Expr b -> Set b;
freeVars (Literal _)     = Set.empty;
freeVars (Tuple xs)      = Set.unions $ freeVars <$> xs;
freeVars (Var v)         = Set.singleton v;
freeVars (Λ cs)          = Set.unions $ (boundVars *** freeVars >>> uncurry (flip Set.difference)) <$> cs;
freeVars (Ply f x)       = freeVars f `Set.union` freeVars x;
freeVars (Note _ x)      = freeVars x;
freeVars (Constructor _) = Set.empty;
freeVars (ForAll v x)    = Set.delete v (freeVars x);

boundVars :: (Ord b) => Match b -> Set b;
boundVars (MatchStruct c ms) = Set.unions $ boundVars <$> ms;
boundVars (MatchTuple ms)    = Set.unions $ boundVars <$> ms;
boundVars (MatchLiteral _)   = Set.empty;
boundVars (MatchAny)         = Set.empty;
boundVars (MatchLazy m)      = boundVars m;
boundVars (MatchAs b m)      = Set.insert b (boundVars m);
boundVars (MatchNote _ m)    = boundVars m;

data TypChkFailure b = InfiniteTypeFailure b (Expr b)
                     | TypeMismatchFailure (Expr b) (Expr b)
                     | MissaturatedConstructorFailure (Constructor b) Int
                     | InalgebraicTypeFailure
                     ;
