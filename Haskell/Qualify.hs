module Haskell.Qualify where

import Control.Category.Unicode;
import Control.Monatron.AutoLift;
import Control.Monatron.Transformer;
import Data.Haskell.Module;
import Data.Haskell.Qualified;
import Data.LTree;
import Data.Map (Map);
import qualified Data.Map as Map;
import Data.R;
import Data.RFunctor;
import Data.Set (Set);
import qualified Data.Set as Set;
import Util;

-- Given this module path, imports, and a qualified name, qualify it wherever it could be
nondeterministicate :: (Ord b) => [b] -> [Import b] -> Q b -> Set (Q b);
nondeterministicate vs = flip $ \ (Q us v) ->
  filter (case us of {
            [] -> not ∘ imp_qual;
            _  -> \ imp -> imp_path imp == us || imp_alias imp == Just us;
          }) &
  fmap (imp_path & Q & flip id v) & Set.fromList &
  case us of { [] -> Set.insert (Q vs v); _ -> id; };
