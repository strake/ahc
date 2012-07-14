module Util where

import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad;
import Control.Monad.Instances;

infixr 3 &=&;
(&=&) :: Monad m => (a -> m b) -> (a -> m c) -> a -> m (b, c);
f &=& g = (liftM2 ∘ liftM2) (,) f g;

infixr 3 *=*;
(*=*) :: Monad m => (a1 -> m b1) -> (a2 -> m b2) -> (a1, a2) -> m (b1, b2);
(f *=* g) (x, y) = liftM2 (,) (f x) (g y);

tripleA :: Arrow a => a b1 c1 -> a b2 c2 -> a b3 c3 -> a (b1, b2, b3) (c1, c2, c3);
tripleA f g h = arr (\ (x, y, z) -> (x, (y, z))) >>> f *** g *** h >>> arr (\ (x, (y, z)) -> (x, y, z));

splitA3 :: Arrow a => a b c1 -> a b c2 -> a b c3 -> a b (c1, c2, c3);
splitA3 f g h = f &&& g &&& h >>> arr (\ (x, (y, z)) -> (x, y, z));

infixr 2 <||>;
(<||>) :: Applicative p => p Bool -> p Bool -> p Bool;
(<||>) = liftA2 (||);

infixr 3 <&&>;
(<&&>) :: Applicative p => p Bool -> p Bool -> p Bool;
(<&&>) = liftA2 (&&);

list :: (a -> [a] -> b) -> b -> [a] -> b;
list f y []     = y;
list f y (x:xs) = f x xs;
