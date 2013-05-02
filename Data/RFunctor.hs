module Data.RFunctor where

import Control.Category.Unicode;
import Data.R;
import GHC.Prim (Constraint);

class R f => RFunctor f where {
  rfmap :: (C f a, C f b) => (a -> b) -> f a -> f b;
};

infixl 4 <$!>;
(<$!>) :: (RFunctor f, C f a, C f b) => (a -> b) -> f a -> f b;
(<$!>) = rfmap;
