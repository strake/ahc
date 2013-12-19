module Data.Fixity where

import Data.Map (Map);

data Fixity = InfixL | InfixR | Infix deriving (Eq, Show);

type FixMap b = Map b (Fixity, Rational);
