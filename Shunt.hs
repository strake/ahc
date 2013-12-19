module Shunt where

import Control.Applicative;
import Control.Category.Unicode;
import Control.Monatron.AutoLift;
import Data.Core;
import Data.Fixity;
import Data.Foldable;
import Data.Function (on);
import qualified Data.Map as Map;
import Data.Ord.Unicode;
import Data.Eq.Unicode;
import Util;

type Arity = Int;

shuntEnd :: (Functor m, ExcM () m) => ([b] -> m [(Arity, Expr b)]) -> m (Expr b);
shuntEnd =
  let {
    go :: [Expr b] -> (Int, Expr b) -> Maybe [Expr b];
    go    xs  (0, y) = Just (y:xs);
    go (x:xs) (n, y) = go xs (n - 1, Ply y x);
    go   _      _    = Nothing;
  } in
  \ xk -> foldlM go [] <$> xk [] >>= \ xs ->
  case xs of {
    Just [x] -> return x;
    _        -> throw ();
  };

shunt0 :: (Monad m) => Expr b -> [b] -> m [(Arity, Expr b)];
shunt0 y ops = return $ (0, y):((,) 2 ∘ Var <$> ops);

shuntGo :: ∀ m b . (Ord b, Applicative m, ReaderM (FixMap b) m, ExcM b m) => b -> Expr b -> ([b] -> m [(Arity, Expr b)]) -> [b] {- operator stack -} -> m [(Arity, Expr b)] {- postfix-order output queue -};
shuntGo u y xk =
  let {
    go :: [b] -> m [(Int, Expr b)];
    go [] = xk (u:[]);
    go (v:vs) =
      (liftA2 (,) `on` lookupFix) u v >>= \ ((f, m), (g, n)) ->
      m ≤ n || m ≡ n && f ≡ InfixR ? (:) (2, Var v) <$> go vs $ xk (u:v:vs);
  } in fmap ((0, y):) ∘ go;

lookupFix :: (Ord b, Functor m, ReaderM (FixMap b) m, ExcM b m) => b -> m (Fixity, Rational);
lookupFix v = Map.lookup v <$> ask >>= maybe (throw v) return;
