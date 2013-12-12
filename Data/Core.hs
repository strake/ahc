{-# LANGUAGE TypeFamilies #-}

module Data.Core where

import Prelude hiding (foldl);
import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad.Gen.Class;
import Data.Foldable;
import qualified Data.List as List;
import qualified Data.Map  as Map;
import Data.Map (Map);
import Data.R;
import Data.RFunctor;

type Type = Expr;

data Expr b = Literal Literal
            | Tuple [Expr b]
            | Var b
            | Λ [(Match b, Expr b)]
            | Ply (Expr b) (Expr b)              -- Function application
            | Let (Map b (Expr b)) (Expr b)
            | Note (Type b) (Expr b)             -- Type annotation
            | Constructor (Constructor b)
            | ForAll b (Expr b)
  deriving (Eq, Show);

data Match b = MatchStruct b [Match b]
             | MatchTuple [Match b]
             | MatchLiteral Literal
             | MatchAny
             | MatchLazy (Match b)
             | MatchAs b (Match b)
             | MatchNote (Type b) (Match b)
  deriving (Eq, Show);

data Constructor b = C b
                   | CStar
                   | CArrow
  deriving (Eq, Show);

data Literal = LInteger Integer
             | LFloat Double
             | LChar Char
             | LChars [Char]
  deriving (Eq, Show);

(-->) :: Expr b -> Expr b -> Expr b;
x --> y = Ply (Ply (Constructor CArrow) x) y;

instance R Expr where {
  type C Expr b = Ord b;
};

instance R Match where {
  type C Match b = Ord b;
};

instance R Constructor where {
  type C Constructor b = Ord b;
};

instance RFunctor Expr where {
  φ `rfmap` Literal l = Literal l;
  φ `rfmap` Tuple xs = Tuple (rfmap φ <$> xs);
  φ `rfmap` Var v = Var (φ v);
  φ `rfmap` Λ ms = Λ ((rfmap φ *** rfmap φ) <$> ms);
  φ `rfmap` Ply f x = Ply (φ <$!> f) (φ <$!> x);
  φ `rfmap` Let bs x = Let (Map.mapKeys φ (rfmap φ <$> bs)) (φ <$!> x);
  φ `rfmap` Note t x = Note (φ <$!> t) (φ <$!> x);
  φ `rfmap` Constructor c = Constructor (φ <$!> c);
};

instance RFunctor Match where {
  φ `rfmap` MatchTuple ms = MatchTuple (rfmap φ <$> ms);
  φ `rfmap` MatchStruct v ms = MatchStruct (φ v) (rfmap φ <$> ms);
  φ `rfmap` MatchLiteral l = MatchLiteral l;
  φ `rfmap` MatchAny = MatchAny;
  φ `rfmap` MatchLazy m = MatchLazy (φ <$!> m);
  φ `rfmap` MatchAs v m = MatchAs (φ v) (φ <$!> m);
  φ `rfmap` MatchNote t m = MatchNote (φ <$!> t) (φ <$!> m);
};

instance RFunctor Constructor where {
  φ `rfmap` C v = C (φ v);
  φ `rfmap` CStar = CStar;
  φ `rfmap` CArrow = CArrow;
};
