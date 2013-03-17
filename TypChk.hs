module TypChk where

import Prelude hiding (foldr, lookup, concatMap);

import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad;
import Data.Core;
import Data.Fallible;
import Data.Foldable;
import Data.Foldable.Unicode;
import Data.Maybe (fromMaybe);
import Data.Map as Map hiding (foldr);
import Data.Set as Set hiding (foldr);
import Data.RFunctor;
import Util;

type Ψ b = Map b (Expr b);
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

unify :: (Ord b) => Σ b -> Fallible [TypChkFailure b] (Ψ b);
unify [] = Work Map.empty;
unify ((Var u, Var v):σ) | u == v = unify σ;
unify ((Var u, y):σ)
  | u ∈ freeVars y = liftA2 Map.union (Fail [InfiniteTypeFailure u y]) (unify σ)
  | otherwise = let {
      ψ = Map.singleton u y;
    } in Map.union ψ <$> fmap (/. ψ) <$> unify σ
  ;
unify ((x, Var v):σ) = unify ((Var v, x):σ);
unify ((Ply f x, Ply g y):σ) = unify ((f, g):(x, y):σ);
unify ((Constructor c1, Constructor c2):σ)
  | c1 == c2  = unify σ
  | otherwise = liftA2 Map.union (Fail [TypeMismatchFailure (Constructor c1) (Constructor c2)]) (unify σ)
  ;
unify ((x, y):σ) = liftA2 Map.union (Fail [TypeMismatchFailure x y]) (unify σ);

δσ :: (Ord b) => [Δ (Either Int b)] -> Σ (Either Int b);
δσ = fmap (fmap pure) & Map.unionsWith (++) & Map.toAscList & zipWith (Left & Var & const & (*** id)) [1..] & concatMap (uncurry distribR);

{-
-- Dr. Gergõ Érdi's algorithm, given in "Compositional Type Checking"
findType :: (Ord b) => Γ b -> Expr b -> Fallible [TypChkFailure] (Δ b, Expr b);
findType γ x = Work 
-}

freeVars :: (Ord b) => Expr b -> Set b;
freeVars (Literal _)     = Set.empty;
freeVars (Var v)         = Set.singleton v;
freeVars (Λ cs)          = Set.unions $ (boundVars *** freeVars >>> uncurry (flip Set.difference)) <$> cs;
freeVars (Ply f x)       = freeVars f `Set.union` freeVars x;
freeVars (Note _ x)      = freeVars x;
freeVars (Constructor _) = Set.empty;
freeVars (ForAll v x)    = Set.delete v (freeVars x);

boundVars :: (Ord b) => Match b -> Set b;
boundVars (MatchStruct c ms) = Set.unions $ boundVars <$> ms;
boundVars (MatchLiteral _)   = Set.empty;
boundVars (MatchAny)         = Set.empty;
boundVars (MatchLazy m)      = boundVars m;
boundVars (MatchAs b m)      = Set.insert b (boundVars m);
boundVars (MatchNote _ m)    = boundVars m;

data TypChkFailure b = InfiniteTypeFailure b (Expr b) | TypeMismatchFailure (Expr b) (Expr b);
