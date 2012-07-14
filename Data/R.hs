{-# LANGUAGE TypeFamilies #-}

module Data.R where

import GHC.Prim (Constraint);

class R (v :: * -> *) where {
  type C v a :: Constraint;
};

instance R [] where {
  type C [] a = ();
};

instance R ((->) b) where {
  type C ((->) b) a = ();
};
