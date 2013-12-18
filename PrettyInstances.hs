module PrettyInstances where

import Data.Core;
import Data.Haskell.NameSpace;
import Data.Haskell.Qualified;
import Data.List as List;
import Data.Text.Pos;
import Haskell.Lex;
import Haskell.Parse;
import TypChk;
import Text.PrettyPrint.Mainland;

instance Pretty b => Pretty (Expr b) where {
  ppr (Literal l) = ppr l;
  ppr (Tuple xs) = ppr xs;
  ppr (Var v) = ppr v;
  --ppr (Λ [(m, x)]) = "(\\ " ++ ppr m ++ " -> " ++ ppr x ++ ")";
  ppr (Ply (Ply (Constructor CArrow) x) y) = ppr x <> text " -> " <> ppr y;
  ppr (Ply f x) = ppr f <> char ' ' <> ppr x;
};

instance Pretty Literal where {
  ppr (LInteger n) = ppr n;
};

instance Pretty TextPos where {
  ppr (TextPos (m, n)) = ppr m <> char ':' <> ppr n;
};

instance Pretty (Q [Char]) where {
  ppr (Q ms n) = text $ List.intercalate "." $ ms ++ [n];
};

instance Pretty ScanFailure where {
  ppr (ScanFailMsg p s) = text "Scan Failure at " <> ppr p <> text ": " <> ppr s;
};

instance Pretty ParseFailure where {
  ppr (ParseFailMsg s) = text "Parse Failure: " <> ppr s;
};

instance Pretty b => Pretty (TFailure b) where {
  ppr (TUnboundVar v) = text "Unbound Variable: " <> ppr v;
  ppr (TMismatch s t) = text "Type Mismatch: " <> ppr s <> text ", " <> ppr t;
};

instance Pretty NameSpace where {
  ppr _ = empty;
};
