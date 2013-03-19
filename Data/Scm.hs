module Data.Scm where

import Prelude hiding (foldr);

import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Data.Core as Core;
import Data.Foldable;
import Data.Map (Map);
import qualified Data.Map as Map;
import Util;

data Scm = ScmPair (Scm, Scm)
         | ScmAtom [Char]
         | ScmEmpty
         ;

instance Show Scm where {
  show (ScmAtom v)      = v;
  show (ScmPair (x, y)) =
    let {
      show' x (ScmPair (y, z)) = show x ++ " " ++ show' y z;
      show' x (ScmEmpty)       = show x;
      show' x y = show x ++ " . " ++ show y;
    } in "(" ++ show' x y ++ ")";
  show (ScmEmpty)       = "()";
};

class ToScm a where {
  toScm :: a -> Scm;
};

instance ToScm [Scm] where {
  toScm = foldr (curry ScmPair) ScmEmpty;
};

instance ToScm (Core.Expr [Char]) where {
  toScm (Literal l)         = toScm l;
  toScm (Tuple xs)          = toScm (ScmAtom "list" : (toScm <$> xs));
  toScm (Var v)             = ScmAtom ("hs_" ++ v);
  toScm (Λ cs)              = toScm (ScmAtom "match-lambda" : (toScm <$> cs));
  toScm (Ply f x)           = toScm [toScm [ScmAtom "force", toScm f], toScm x];
  toScm (Let bm x)          = toScm [ScmAtom "letrec", toScm ((ScmAtom ∘ ("hs_" ++) *** (\ x -> toScm [ScmAtom "delay", toScm x]) & (:[]) & toScm >>> ScmPair) <$> Map.toList bm), toScm x];
  toScm (Note _ x)          = toScm x;
  toScm (Constructor (C v)) = toScm (Var v); -- must be defined as scm function
};

instance ToScm Literal where {
  toScm (LInteger n) = ScmAtom (show n);
};

instance ToScm (Match [Char]) where {
  toScm (MatchStruct v ms) = toScm [ScmAtom "!", toScm $ ScmAtom ("\'hs_" ++ v) : (toScm <$> ms)];
  toScm (MatchTuple ms)    = toScm [ScmAtom "!", toScm $ toScm <$> ms];
  toScm (MatchLiteral l)   = toScm [ScmAtom "!", toScm l];
  toScm (MatchAny)         = ScmAtom "_";
  toScm (MatchAs v m)      = toScm [ScmAtom "and", ScmAtom ("hs_" ++ v), toScm m];
  toScm (MatchNote _ m)    = toScm m;
};

instance ToScm (Match [Char], Core.Expr [Char]) where {
  toScm (m, x) = toScm [toScm m, toScm x];
};

scmCurryN :: Int -> Scm -> Scm;
scmCurryN n x = foldr (\ k x -> toScm [ScmAtom "lambda", toScm [ScmAtom ('x' : show k)], x]) (toScm $ x : (ScmAtom ∘ ('x':) ∘ show <$> [1..n])) [1..n];
