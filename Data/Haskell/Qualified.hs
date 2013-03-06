module Data.Haskell.Qualified where

import Control.Applicative;
import Control.Category.Unicode;
import Data.List as List;
import Data.String;

data Q b = Q [b] b
  deriving (Eq, Ord, Functor);

instance IsString b => IsString (Q b) where { fromString = Q [] ∘ fromString; };

instance Show (Q [Char]) where {
  show (Q ms n) = List.intercalate "." $ show <$> ms ++ [n];
};
