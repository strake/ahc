module Util.Monatron where

import Prelude hiding (fail);

import Control.Applicative;
import Control.Category.Unicode;
import Control.Monatron.AutoLift hiding (fail);
import Control.Monatron.Transformer hiding (fail);
import Util;

evalStateT = fmap fst ∘∘ runStateT;

modify f = get >>= put ∘ f;
