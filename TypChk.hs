{-# LANGUAGE TypeFamilies #-}

module TypChk where

import Prelude hiding (foldr, sequence);

import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad hiding (sequence);
import Control.Monatron.AutoLift hiding (sequence);
import Control.Monatron.Transformer hiding (sequence);
import Control.Monad.Gen.Class;
import Data.Core;
import Data.Foldable;
import Data.Foldable.Unicode;
import Data.Map (Map);
import qualified Data.Map as Map;
import Data.Maybe;
import Data.Monoid;
import Data.R;
import Data.RFunctor;
import Data.Set (Set);
import qualified Data.Set as Set;
import Data.Traversable;
import Util;
import qualified Util.Map as Map;
import Util.Monatron;

data TFailure b = TUnboundVar b | TMismatch (Type b) (Type b) deriving (Show);

instance R TFailure where {
  type C TFailure b = Ord b;
};

instance RFunctor TFailure where {
  rfmap f (TUnboundVar v) = TUnboundVar (f v);
  rfmap f (TMismatch s t) = TMismatch (rfmap f s) (rfmap f t);
};

data TR b = TR {
  r_env :: Map b (Type b),
  r_svs :: Set b, -- specific type variables
  r_typeLiteral :: Literal -> Type b
};

r_onEnv :: (Map b (Type b) -> Map b (Type b)) -> TR b -> TR b;
r_onEnv f tr@(TR { r_env = env }) = tr { r_env = f env };

r_onSvs :: (Set b -> Set b) -> TR b -> TR b;
r_onSvs f tr@(TR { r_svs = svs }) = tr { r_svs = f svs };

data TW b = TW {
  w_eqn :: [(Type b, Type b)]
} deriving (Show);

instance Monoid (TW b) where {
  mempty = TW [];
  TW xs `mappend` TW ys = TW (xs ++ ys);
};

infer :: ∀ m b . (Ord b, Applicative m, ExcM (TFailure b) m, MonadGen b m, ReaderM (TR b) m, WriterM (TW b) m) => Expr b -> m (Type b);
infer (Note t x) = infer x >>= unify t;
infer (Var v)    = Map.lookup v ∘ r_env <$> ask >>= maybe (throw (TUnboundVar v)) freshen;
infer (Tuple xs) = Tuple <$> traverse infer xs;
infer (Λ cs)     = traverse (\ (m, x) -> inferM m >>= \ (env, svs, t) ->
                             (t -->) <$> local ((r_onEnv $ Map.union env) ∘ (r_onSvs $ Set.union svs)) (infer x)) cs >>= list (error "empty λ") (foldrM unify);
infer (Ply f x)  = liftA2 (,) (infer f) (liftA2 (-->) (infer x) (Var <$> gen)) >>= uncurry unify;
infer (Let bm x) = traverse (const gen) bm >>= \ fvm {- fresh variable map -} ->
                   local (r_onEnv $ Map.union (Var <$> fvm)) $
                   local (r_onSvs $ Set.union (foldMap Set.singleton fvm)) (traverse infer bm >>= Map.unionWithA unify (Var <$> fvm)) >>= \ env ->
                   local (r_onEnv $ Map.union env) (infer x);
infer (Constructor (C v)) = infer (Var v);
infer (Literal l) = flip r_typeLiteral l <$> ask;

-- Matches must be linear by now
inferM :: ∀ m b . (Ord b, Applicative m, ExcM (TFailure b) m, MonadGen b m, ReaderM (TR b) m, WriterM (TW b) m) => Match b -> m (Map b (Type b) {- types of bound vars -}, Set b {- specific type vars -}, Type b);
inferM (MatchNote s m)    = inferM m >>= \ (env, svs, t) -> (,,) env svs <$> unify s t;
inferM (MatchAs v m)      = (\ (env, svs, t) -> (Map.insert v t env, svs, t)) <$> inferM m;
inferM (MatchAny)         = splitA3 (const Map.empty) Set.singleton Var <$> gen;
inferM (MatchTuple ms)    = unzip3 & tripleA Map.unions Set.unions Tuple <$> traverse inferM ms;
inferM (MatchLazy m)      = inferM m;
inferM (MatchStruct w ms) = Map.lookup w ∘ r_env <$> ask >>= maybe (throw (TUnboundVar w)) freshen >>= \ t -> gen >>= \ v ->
                            traverse inferM ms >>= unzip3 & tripleK (return ∘ Map.unions) (return ∘ Set.insert v ∘ Set.unions) ((Var v <$) ∘ unify t ∘ foldr (-->) (Var v));
inferM (MatchLiteral l)   = (,,) Map.empty Set.empty ∘ flip r_typeLiteral l <$> ask;

unify :: (WriterM (TW b) m) => Type b -> Type b -> m (Type b);
unify s t = tell (TW [(s, t)]) >> return s; -- lazy method (^_^)

freshen :: (Ord b, Applicative m, MonadGen b m, ReaderM (TR b) m, WriterM (TW b) m) => Type b -> m (Type b);
freshen t = r_svs <$> ask >>= \ svs ->
  (\ fvm -> rfmap (join $ flip Map.lookup fvm & maybe id const) t) <$>
  (Set.toAscList & zipWith (flip $ fmap ∘ (,)) (repeat gen) & sequence & fmap Map.fromAscList) (freeVars t `Set.difference` svs);

findUnifier :: ∀ m b . (Ord b, Applicative m, ExcM (TFailure b) m, ReaderM (TR b) m) => [(Type b, Type b)] -> m (Map b (Type b));
findUnifier [] = return Map.empty;
findUnifier ((x, y):eqns) | x == y      = findUnifier eqns;
findUnifier ((Tuple xs, Tuple ys):eqns)
               | length xs == length ys = findUnifier (zip xs ys ++ eqns);
findUnifier ((Ply f x, Ply g y):eqns)   = findUnifier ((f, g):(x, y):eqns);
findUnifier ((Var v, x):eqns)
                       | v ∉ freeVars x = Map.insert v x <$> findUnifier (join (***) (/. Map.singleton v x) <$> eqns);
findUnifier ((x, Var v):eqns)           = findUnifier ((Var v, x):eqns);
findUnifier ((x, y):eqns) = throw (TMismatch x y);

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

(/.) :: (Ord b) => Expr b -> Map b (Expr b) -> Expr b;
Var v         /. ψ = fromMaybe (Var v) (Map.lookup v ψ);
Tuple xs      /. ψ = Tuple ((/. ψ) <$> xs);
Λ cs          /. ψ = Λ ((\ (m, x) -> (m, x /. foldr Map.delete ψ (boundVars m))) <$> cs);
Ply f x       /. ψ = Ply (f /. ψ) (x /. ψ);
Let bs x      /. ψ = error "type-level let";
Note t x      /. ψ = Note t (x /. ψ);
Constructor c /. ψ = Constructor c;
ForAll v x    /. ψ = ForAll v (x /. Map.delete v ψ);
