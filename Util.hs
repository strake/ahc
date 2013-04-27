module Util where

import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad;
import Control.Monad.Instances;
import Data.List (partition);

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

infix 1 ?;
(?) :: Bool -> a -> a -> a;
True  ? x = const x;
False ? _ = id;

whileJust :: (Functor m, Monad m) => m (Maybe a) -> (a -> m b) -> m [b];
whileJust mmx f = mmx >>= maybe (return []) (f >=> flip fmap (whileJust mmx f) ∘ (:));

infixr 9 &;
(&) :: (a -> b) -> (b -> c) -> (a -> c);
(&) = flip (∘);

distribLWith :: Functor v => (a -> b -> c) -> v a -> b -> v c;
distribLWith = flip ∘ distribRWith ∘ flip;

distribRWith :: Functor v => (a -> b -> c) -> a -> v b -> v c;
distribRWith f x yv = f x <$> yv;

distribL :: Functor v => v a -> b -> v (a, b);
distribL = distribLWith (,);

distribR :: Functor v => a -> v b -> v (a, b);
distribR = distribRWith (,);

factorizeLBy :: (a -> a -> Bool) -> [(a, b)] -> [(a, [b])];
factorizeLBy (==) = list (\ (u, v) -> partition ((== u) ∘ fst) >>> fmap snd & (,) u *** factorizeLBy (==) >>> uncurry (:)) [];

factorizeRBy :: (b -> b -> Bool) -> [(a, b)] -> [([a], b)];
factorizeRBy (==) = fmap swap ∘ factorizeLBy (==) ∘ fmap swap;

factorizeL :: Eq a => [(a, b)] -> [(a, [b])];
factorizeL = factorizeLBy (==);

factorizeR :: Eq b => [(a, b)] -> [([a], b)];
factorizeR = factorizeRBy (==);

swap :: (a, b) -> (b, a);
swap (x, y) = (y, x);
