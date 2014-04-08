{-# LANGUAGE ViewPatterns #-}

module Util.Map where

import Prelude hiding (foldr, lookup, null);

import Control.Applicative;
import Control.Category.Unicode;
import Control.Monad;
import Data.Foldable;
import Data.Function (on);
import qualified Data.List as List;
import Data.Maybe (mapMaybe);
import Data.Map (Map);
import qualified Data.Map as Map;
import Data.Set (Set);
import qualified Data.Set as Set;
import Data.Traversable;
import Util;

unionWithA :: (Ord k, Applicative p) => (a -> a -> p a) -> Map k a -> Map k a -> p (Map k a);
unionWithA f = traverse (either pure id) ∘∘ unionEither f;

unionEither :: (Ord k) => (a -> a -> b) -> Map k a -> Map k a -> Map k (Either a b);
unionEither f = Map.unionWith (\ (Left x) (Left y) -> Right (f x y)) `on` fmap Left;

fromListUniqM :: (Ord k, Monad m) => (∀ b . [k] -> m b) -> [(k, a)] -> m (Map k a);
fromListUniqM f kvs
  | ks@(_:_) <- fmap fst & List.sort & List.group & mapMaybe (list empty ((<$) & (∘ list empty (pure Just)))) $ kvs = f ks
  | otherwise = return (Map.fromList kvs);
