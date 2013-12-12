module Util.Stream where

import Prelude hiding (concatMap);

import qualified Data.List as List;
import Data.Stream (Stream (..));
import qualified Data.Stream as Stream;

concatMap :: (a -> [a]) -> Stream a -> Stream a;
concatMap f (Cons x xs) = f x ++> concatMap f xs;

infixr 5 ++>;
(++>) :: [a] -> Stream a -> Stream a;
(++>) = flip (foldr Cons);
