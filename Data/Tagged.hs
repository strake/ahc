module Data.Tagged where

import Control.Applicative;
import Data.Monoid;

data Tagged t a = T t a
  deriving (Eq, Functor, Show);

instance Monoid t => Applicative (Tagged t) where {
  pure = T mempty;
  T s f <*> T t x = T (s <> t) (f x);
};
