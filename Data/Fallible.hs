module Data.Fallible where

import Control.Applicative;
import Control.Monad;
import Data.Either;
import Data.Monoid;

data Fallible e a = Fail e | Work a deriving (Functor);

instance Monoid e => Applicative (Fallible e) where {
  pure = Work;
  Fail e <*> Fail f = Fail (e <> f);
  Fail e <*> _      = Fail  e;
  _      <*> Fail f = Fail  f;
  Work f <*> Work x = Work (f x);
};

instance Monad (Fallible e) where {
  return = Work;
  Fail e >>= _ = Fail e;
  Work x >>= f = f x;
};
