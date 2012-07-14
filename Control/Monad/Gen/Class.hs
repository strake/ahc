module Control.Monad.Gen.Class where

import Control.Monad;

class Monad m => MonadGen b m where {
  gen :: m b;
};

instance MonadGen b m => MonadGen (b, b) m where {
  gen = liftM2 (,) gen gen;
};
