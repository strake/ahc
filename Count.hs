module Count where

import Prelude hiding (take, repeat, concatMap, cycle, zipWith);
import Control.Applicative;
import Control.Category.Unicode;
import Data.Monoid;
import Data.Stream as Stream;
import Util.Stream as Stream;

countl :: [a] -> Stream [a];
countl xs = (pure <$> xs) ++> zipWith (++) (cycle (pure <$> xs)) (concatMap (take (length xs) ∘ repeat) (countl xs));

countr :: [a] -> Stream [a];
countr xs = (pure <$> xs) ++> zipWith (++) (concatMap (take (length xs) ∘ repeat) (countr xs)) (cycle (pure <$> xs));
