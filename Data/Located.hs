module Data.Located where

import Control.Applicative;
import Data.Tagged;
import Data.Text.Pos;

type Located b = Tagged ConvexTextRange b;
