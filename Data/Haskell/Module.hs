module Data.Haskell.Module where

import Data.Core;
import Data.Functor.Identity;
import Data.Haskell.Qualified;
import Data.Haskell.NameSpace;
import Data.Map;

data Module v b = Module [Import b] [Export b] (Map (Maybe NameSpace, Q b) (v (Maybe NameSpace, Q b)));

data Import b = Import {
  imp_qual :: Bool,
  imp_path :: [b],
  imp_alias :: Maybe [b],
  imp_spec :: ImpSpec b
};

data Export b = ExportSymbol (PortSpec Q b) | ExportModule [b];

data ImpSpec b = Showing [PortSpec Identity b] | Hiding [PortSpec Identity b];

data PortSpec q b = Port1 (q b) | PortSome (q b) [b] | PortAll (q b);
