module Util.MapTree where

import Control.Applicative;
import Control.Category.Unicode;
import Data.LTree;
import Data.Map (Map);
import qualified Data.Map as Map;

mapWithDeepLKey :: ([k] -> a -> b) -> LTree (Map k) a -> LTree (Map k) b;
mapWithDeepLKey f (Leaf x) = Leaf (f [] x);
mapWithDeepLKey f (Stem tm) = Stem (Map.mapWithKey (mapWithDeepLKey ∘ (f ∘) ∘ (:)) tm);
