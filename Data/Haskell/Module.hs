module Data.Haskell.Module where

import Data.Core;
import Data.Haskell.Qualified;
import Data.Map;

data Module v b = Module [Import b] [Export b] (Map b (v (Q b)));

data Import b = Import {
  imp_qual :: Bool,
  imp_path :: [b],
  imp_alias :: Maybe [b],
  imp_spec :: ImpSpec b
};

data Export b = ExportSymbol (PortSpec (Q b)) | ExportModule [b];

data ImpSpec b = Showing [PortSpec b] | Hiding [PortSpec b];

data PortSpec b = Port1 b | PortSome b [b] | PortAll b;
