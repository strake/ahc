module Control.Monad.Gen.Class where

import Control.Applicative;
import Control.Arrow;
import Control.Monad;
import Control.Monad.State.Class;
import Data.Stream (Stream (..));
import qualified Data.Stream as Stream;
import Util;

class Monad m => MonadGen b m where {
  gen :: m b;
};

instance MonadGen b m => MonadGen (b, b) m where {
  gen = liftM2 (,) gen gen;
};

instance (Applicative m, MonadState (Stream b) m) => MonadGen b m where {
  gen = state $ \ (Cons x xs) -> (x, xs);
};
