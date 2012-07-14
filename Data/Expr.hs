{-# LANGUAGE TypeFamilies #-}

module Data.Expr where

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

type Type b = Expr b;

data Expr b = Literal Literal
            | Var b
            | Λ [(Match b, Expr b)]
            | Ply (Expr b) (Expr b)              -- Function application
            | Let (Map b (Expr b)) (Expr b)
            | Note (Type b) (Expr b)             -- Type annotation
            | Constructor (Constructor b)
  deriving Eq;

data Match b = MatchStruct b [Match b]
             | MatchLiteral Literal
             | MatchAny
             | MatchLazy (Match b)
             | MatchAs b (Match b)
             | MatchNote (Type b) (Match b)
  deriving Eq;

data Constructor b = C [Type b] (Type b)
                   | CStar
                   | CArrow
  deriving Eq;

cType :: Constructor b -> Type b;
cType (C argus final) = List.foldr (-->) final argus;

mkCTuple :: (Functor m, MonadGen b m) => Int -> m (Constructor b);
mkCTuple n = (\ vs ->
              C (Var <$> vs) (foldl Ply (Constructor $
                                         C (take n ∘ repeat $
                                            Constructor CStar) (Constructor CStar)) (Var <$> vs))) <$>
             (sequence ∘ take n ∘ repeat) gen;

data Literal = LInteger Integer
             | LFloat Double
             | LChar Char
             | LChars [Char]
  deriving (Eq, Show);

(-->) :: Expr b -> Expr b -> Expr b;
x --> y = Ply (Ply (Constructor CArrow) x) x;

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
  φ `rfmap` Var v = Var (φ v);
  φ `rfmap` Λ ms = Λ ((rfmap φ *** rfmap φ) <$> ms);
  φ `rfmap` Ply f x = Ply (φ <$!> f) (φ <$!> x);
  φ `rfmap` Let bs x = Let (Map.mapKeys φ (rfmap φ <$> bs)) (φ <$!> x);
  φ `rfmap` Note t x = Note (φ <$!> t) (φ <$!> x);
  φ `rfmap` Constructor c = Constructor (φ <$!> c);
};

instance RFunctor Match where {
  φ `rfmap` MatchStruct c ms = MatchStruct (φ c) (rfmap φ <$> ms);
  φ `rfmap` MatchLiteral l = MatchLiteral l;
  φ `rfmap` MatchAny = MatchAny;
  φ `rfmap` MatchLazy m = MatchLazy (φ <$!> m);
  φ `rfmap` MatchAs v m = MatchAs (φ v) (φ <$!> m);
  φ `rfmap` MatchNote t m = MatchNote (φ <$!> t) (φ <$!> m);
};

instance RFunctor Constructor where {
  φ `rfmap` C as x = C (rfmap φ <$> as) (φ <$!> x);
  φ `rfmap` CStar = CStar;
  φ `rfmap` CArrow = CArrow;
};
