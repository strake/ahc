{-# LANGUAGE ViewPatterns #-}

module Haskell.Parse where

import Prelude hiding (foldr, foldr1, sequence);
import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad hiding (sequence);
import Control.Monad.Gen.Class;
import Control.Monatron.AutoLift hiding (sequence);
import Control.Monatron.Monad;
import Control.Monatron.Transformer hiding (sequence);
import Data.Either;
import Data.Fixity;
import Data.Eq.Unicode;
import Data.Ord.Unicode;
import Data.Core hiding (LInteger, LFloat, LChar, LChars);
import qualified Data.Core as P;
import Data.Foldable;
import Data.Traversable;
import Data.Function (on);
import Data.Haskell.NameSpace;
import Data.Haskell.Token as T;
import Data.Haskell.Qualified;
import qualified Data.List as List hiding (foldr1);
import Data.Map (Map);
import qualified Data.Map as Map;
import Data.Set (Set);
import qualified Data.Set as Set;
import Data.Maybe;
import Data.Stream;
import Shunt;
import Util;
import Util.Map;

%{

Terminal	= LParenth as '(' | RParenth as ')'
		| LBracket as '[' | RBracket as ']'
		| LBrace   as '{' | RBrace   as '}'
		| SemiColon as ';' | Comma as ',' | BackTick as '`'

		| Syntactosem "."		as "."
		| Syntactosem ".."		as ".."
		| Syntactosem "::"		as "::"
		| Syntactosem "="		as "="
		| Syntactosem "\\"		as "\\"
		| Syntactosem "|"		as "|"
		| Syntactosem "<-"		as "<-"
		| Syntactosem "->"		as "->"
		| Syntactosem "@"		as "@"
		| Syntactosem "~"		as "~"
		| Syntactosem "=>"		as "=>"
		| Syntactosem "case"		as "case"
		| Syntactosem "class"		as "class"
		| Syntactosem "data"		as "data"
		| Syntactosem "deriving"	as "deriving"
		| Syntactosem "foreign"		as "foreign"
		| Syntactosem "import"		as "import"
		| Syntactosem "in"		as "in"
		| Syntactosem "infix"		as "infix"
		| Syntactosem "infixl"		as "infixl"
		| Syntactosem "infixr"		as "infixr"
		| Syntactosem "instance"	as "instance"
		| Syntactosem "let"		as "let"
		| Syntactosem "λ"		as "λ"
		| Syntactosem "module"		as "module"
		| Syntactosem "newtype"		as "newtype"
		| Syntactosem "of"		as "of"
		| Syntactosem "type"		as "type"
		| Syntactosem "where"		as "where"
		| Syntactosem "_"		as "_"

		| LInteger { Integer } as "<integer>"
		| LFloat   { Double  } as "<float>"
		| LChar    { Char    } as "<character>"
		| LChars   { [Char]  } as "<character string>"

		| VarName { [Char] } as "name" | VarSymbol { [Char] } as "symbol"
		| TypName { [Char] } as "Name" | TypSymbol { [Char] } as "Symbol"
		;

*expr		{ PT Fixed (Expr HsName) };
 expr		{ runExcT >=> either (throw ∘ \ () -> ParseFailMsg "malformed infix expression") return $
		  shuntEnd (lift ∘ xk) }					: infixexpr_ { xk };

infixexpr_	{ [HsName] {- operator stack -} -> PT Fixed [(Arity, Expr HsName)] {- postfix-order output queue -} };
infixexpr_	{ fufM ∘ (lift y >>=) ∘ flip (flip (shuntGo u) (lift ∘ xk)) }	: infixexpr_ { xk }, qop { (,) (Just TermName) -> u }, λexpr { y };
		{ fufM ∘ (lift x >>=) ∘ flip (lift ∘∘ shunt0) }			| λexpr { x };

λexpr		{ PT Fixed (Expr HsName) };
λexpr		{ x }								: fexpr { x };
		{ liftA2 Let ds (local (Map.union fm) x) }			| "let", '{', decls { (fm, ds) }, '}', "in", expr { x };
		{ liftA2 (foldr (\ m x -> Λ [(m, x)])) x ms }			| "\\", many amatch { sequence -> ms }, "->", expr { x };
		{ liftA2 Ply (Λ <$> sequence as) x }				| "case", expr { x }, "of", '{', sepEndBy alt ';' { as }, '}';

alt		{ PT Fixed (Match HsName, Expr HsName) };
alt		{ liftA2 (,) m x }						: match { m }, "->", expr { x };

fexpr		{ PT Fixed (Expr HsName) };
fexpr		{ maybe id (liftA2 Ply) m_f x }					: opt fexpr { m_f }, aexpr { x };

aexpr		{ PT Fixed (Expr HsName) };
aexpr		{ return $ Var (Just TermName, v) }				: qvar { v };
		{ return $ Literal l }						| literal { l };
		{ stlist id Tuple <$> xs }					| '(', sepBy expr ',' { sequence -> xs }, ')';
		{ Ply (Var (Just TermName, v)) <$> x }				| '(', expr { x }, qop { v }, ')';
		{% gen >>= \ u -> return $
		   (\ x ->
		    Λ [(MatchAs u MatchAny,
		        Ply (Ply (Var (Just TermName, v)) (Var u)) x)]) <$> x }	| '(', qop { v }, expr { x }, ')';

decls		{ (Map HsName (Fixity, Rational), PT Fixed (Map HsName (Expr HsName))) };
decls		{% let {
		     --mkUniqMapOf :: (Ord k, Show k, MonadFailure ParseFailure m) => [Char] -> [(k, a)] -> m (Map k a);
		     mkUniqMapOf s kvs = fromListUniqM (\ ks -> failHere $ ParseFailMsg ("Multique " ++ s ++ " of " ++ (List.intercalate ", " ∘ fmap show) ks)) kvs;

		     -- include type signatures in term definitions
		     -- check for multique signatures and signatures with no term defined
		     --typSign :: (Ord b, Show b, MonadFailure ParseFailure m) => Map b (Expr b) -> Map b (Expr b) -> m (Map b (Expr b));
		     typSign tm dm | Set.size tsnd > 0 = failHere $ ParseFailMsg ("Type signature but no definition of " ++ (List.intercalate ", " $ show <$> Set.toList tsnd))
                                   | otherwise = return $ Map.unionWith (flip Note) dm tm
		       where {
		         tsnd = Map.keysSet tm Set.\\ Map.keysSet dm;
		       };

		     fs :: [(HsName, (Fixity, Rational))];
		     f_tds :: PT Fixed ([(HsName, Expr HsName)], [(HsName, Expr HsName)]);
		     (fs, f_tds) = let {
		                     go :: ([β] -> c) -> [Either ([a], b) β] -> ([(a, b)], c);
		                     go f = partitionEithers >>> x *** f;
		                     
		                     x = fmap (uncurry distribL) & join;
		                   } in go (sequence & fmap (go x)) ds;
		   }
		   in mkUniqMapOf "fixity declaration" fs >>= \ fm -> return ∘ (,) fm $
		      local (Map.union fm) f_tds >>=
		      mkUniqMapOf "type signature" *=* mkUniqMapOf "term definition" >>=
		      uncurry typSign
		      
		}								: sepEndBy decl ';' { ds };

decl		{ Either ([HsName], (Fixity, Rational)) (PT Fixed
		                                         (Either
		                                          ([HsName], Expr HsName) {- Type Signature  -}
		                                          ([HsName], Expr HsName) {- Term Definition -})) };
decl		{ d }								: gdecl { d };
		{ Right (Right ∘ (,) [(Just TermName, Q [] v)] <$> x) }		| termvar { v }, "=", expr { x };

fnclause	{ PT Fixed (HsName, [Match HsName], Expr HsName) };
fnclause	{ liftA2 (\ (x, y) z -> (x, y, z)) lhs rhs }			: fnlhs { lhs }, rhs { rhs };

fnlhs		{ PT Fixed (HsName, [Match HsName]) };
fnlhs		{ (,) (Just TermName, v) <$> ms }				: termvar { v }, many amatch { sequence -> ms };
		{ (,) (Just TermName, v) <$> sequence [m1, m2] }		| match { m1 }, termop { v }, match { m2 };
		{ lhs >>= return *=* flip fmap ms ∘ flip (++) }			| '(', fnlhs { lhs }, ')', many amatch { sequence -> ms };

rhs		{ PT Fixed (Expr HsName) };
rhs		{ x }								: "=", expr { x };

gdecl		{ ∀ a .
		  Either ([HsName], (Fixity, Rational)) (PT Fixed
		                                         (Either
		                                          ([HsName], Expr HsName) {- Type Signature -} a)) };
gdecl		{ Left ((,) (Just TermName) <$> vs, (fs, fromIntegral n)) }	: fixity { fs }, "<integer>" { n }, many qop { vs };
		{ Right $ Left ∘ (,) ((,) (Just TermName) <$> vs) <$> t }	| sepBy termvar ',' { fmap (Q []) -> vs }, "::", type { t };

type		{ PT Fixed (Type HsName) };
type		{ t }								: btype { t };
		{ liftA2 Ply (Ply (Constructor CArrow) <$> s) t }		| btype { s }, "->" , type { t };

btype		{ PT Fixed (Type HsName) };
btype		{ foldr1 Ply <$> ts }						: some atype { sequence -> ts };

atype		{ PT Fixed (Type HsName) };
atype		{ return $ Var (Just TypeName, v) }				: qvar { v };
		{ stlist id Tuple <$> ts }					| '(', sepBy type ',' { sequence -> ts }, ')';
		{ return $ Constructor CArrow }					| '(', "->", ')';

match		{ PT Fixed (Match HsName) };
match		{ m }								: lmatch { m };

lmatch		{ PT Fixed (Match HsName) };
lmatch		{ m }								: amatch { m };

amatch		{ PT Fixed (Match HsName) };
amatch		{ return $ MatchAny }						: "_";
		{ return $ MatchLiteral l }					| literal { l };
		{ MatchAs (Just TermName, Q [] v) <$>
		  fromMaybe (return MatchAny) m_m }				| termvar { v }, opt as { m_m };
		{ stlist id MatchTuple <$> ms }					| '(', sepBy match ',' { sequence -> ms }, ')';

as		{ PT Fixed (Match HsName) };
as		{ m }							: "@", amatch { m };

qop		{ Q [Char] };
qop		{ v }			: q "symbol" { v };
		{ v }			| q "Symbol" { v };
		{ v }			| '`', q "name" { v }, '`';
		{ v }			| '`', q "Name" { v }, '`';

qvar		{ Q [Char] };
qvar		{ v }			: q "name" { v };
		{ v }			| q "Name" { v };
		{ v }			| '(', q "symbol" { v }, ')';
		{ v }			| '(', q "Symbol" { v }, ')';

termvar		{ [Char] };
termvar		{ v }			: "name" { v };
		{ v }			| '(', "symbol" { v }, ')';

termop		{ [Char] };
termop		{ v }			: "symbol" { v };
		{ v }			| '`', "name" { v }, '`';

q x		{ Q [Char] }		<- x { [Char] };
q x		{ Q ms n }		: sepBy "Name" "." { ms }, x { n };

literal		{ Literal };
literal		{ P.LInteger n  }	: "<integer>"		{ n  };
		{ P.LFloat   x  }	| "<float>"		{ x  };
		{ P.LChar    x  }	| "<character>"		{ x  };
		{ P.LChars   xs }	| "<character string>"	{ xs };

fixity		{ Fixity };
fixity		{ InfixL }		: "infixl";
		{ InfixR }		| "infixr";
		{ Infix  }		| "infix";

sepEndBy x s { [a] } <- x { a }, s;
sepEndBy x s { [] }		:;
             { xs ++ [x] }	| sepEndBy x s { xs }, x { x }, s;

some x { [a] } <- x { a };
some x { [x] }			: x { x };
       { xs ++ [x] }		| some x { xs }, x { x };

}%

frown ts = failHere $ ParseFailMsg ("Parse Failure: " ++ show ts);

type HsName = (Maybe NameSpace, Q [Char]);

type PSt = Stream HsName;

type FixedT = ReaderT (FixMap HsName);

type Fixed = FixedT Id;

type PT m = ExcT ParseFailure (StateT PSt m);

type P = PT Id;

data ParseFailure = ParseFailMsg [Char];

instance Show ParseFailure where {
  show (ParseFailMsg s) = "Parse Failure: " ++ s;
};

fufM = runExcT >=> either (throw ∘ \ (_, v :: Q [Char]) -> ParseFailMsg ("unknown fixity: " ++ show v)) return

failHere = throw;

stlist :: (a -> b) -> ([a] -> b) -> [a] -> b;
stlist f g [x] = f x;
stlist f g  xs = g xs;
