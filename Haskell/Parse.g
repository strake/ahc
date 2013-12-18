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
import Data.Functor.Identity;
import Data.Eq.Unicode;
import Data.Ord.Unicode;
import Data.Core hiding (LInteger, LFloat, LChar, LChars);
import qualified Data.Core as P;
import Data.Foldable;
import Data.Traversable;
import Data.Function (on);
import Data.Haskell.Module;
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

*parseModule	{ ([[Char]], Map HsName (Fixity, Rational), PT Fixed (Module Expr [Char])) };
 parseModule	{ (path, fm, Module imps exps <$> body) }	: "module", modName { path }, opt (bracket '(' (sepMaybeEndBy export ',') ')') { fromMaybe [] -> exps }, "where", modBody { (imps, fm, body) };

modBody		{ ([Import [Char]], Map HsName (Fixity, Rational), PT Fixed (Map HsName (Expr HsName))) };
modBody		{ (imps, fm, body) }				: sepEndBy impdecl ';' { imps }, decls { (fm, body) };
		{ (imps, Map.empty, return Map.empty) }		| sepBy    impdecl ';' { imps };

export		{ Export [Char] };
export		{ ExportSymbol (Port1 v) }			: qvar { v };
		{ ExportSymbol (PortSome v vs) }		| qtypevar { v }, '(', sepMaybeEndBy var ',' { vs }, ')';
		{ ExportSymbol (PortAll v) }			| qtypevar { v }, '(', "..", ')';
		{ ExportModule path }				| "module", modName { path };

impdecl		{ Import [Char] };
impdecl		{ Import {
		    imp_qual  = qual,
		    imp_path  = path,
		    imp_alias = alias,
		    imp_spec  = spec
		  } }						: "import", opt qual { maybe False (pure True) -> qual }, modName { path }, opt alias { alias }, impspec { spec };

qual		{ () };
qual		{ () }						: VarName { "qualified" };

alias		{ [[Char]] };
alias		{ path }					: VarName { "as" }, modName { path };

impspec		{ ImpSpec [Char] };
impspec		{ Hiding [] }					:;
		{ (case hiding of {
		     Just _ -> Hiding;
		     _      -> Showing;
                   }) imps }					| opt hiding { hiding }, '(', sepMaybeEndBy import ',' { imps }, ')';

hiding		{ () };
hiding		{ () }						: VarName { "hiding" };

import		{ PortSpec Identity [Char] };
import		{ Port1 v }					: var { Identity -> v };
		{ PortSome v vs }				| typevar { Identity -> v }, '(', sepMaybeEndBy var ',' { vs }, ')';
		{ PortAll v }					| typevar { Identity -> v }, '(', "..", ')';

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
		     (fs, f_tds) = partitionDecls ds;
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

qvar		{ Q [Char] };
qvar		{ v }			: prefixNota (either (q "name") (q "Name")) (either (q "symbol") (q "Symbol")) { either id id -> v };

qtypevar	{ Q [Char] };
qtypevar	{ v }			: prefixNota (q "Name") (q "Symbol") { v };

qop		{ Q [Char] };
qop		{ v }			: infixNota (either (q "name") (q "Name")) (either (q "symbol") (q "Symbol")) { either id id -> v };

var		{ [Char] };
var		{ v }			: either termvar typevar { either id id -> v };

termvar		{ [Char] };
termvar		{ v }			: prefixNota "name" "symbol" { v };

typevar		{ [Char] };
typevar		{ v }			: prefixNota "Name" "Symbol" { v };

termop		{ [Char] };
termop		{ v }			: infixNota "name" "symbol" { v };

q x		{ Q [Char] }		<- x { [Char] };
q x		{ Q ms n }		: modName { ms }, x { n };

modName		{ [[Char]] };
modName		{ ms }			: sepBy "Name" "." { ms };

prefixNota x y	{ b } <- x { b }, y { b };
prefixNota x y	{ v }			: x { v };
		{ v }			| '(', y { v }, ')';

infixNota x y	{ b } <- x { b }, y { b };
infixNota x y	{ v }			: y { v };
		{ v }			| '`', x { v }, '`';

literal		{ Literal };
literal		{ P.LInteger n  }	: "<integer>"		{ n  };
		{ P.LFloat   x  }	| "<float>"		{ x  };
		{ P.LChar    x  }	| "<character>"		{ x  };
		{ P.LChars   xs }	| "<character string>"	{ xs };

fixity		{ Fixity };
fixity		{ InfixL }		: "infixl";
		{ InfixR }		| "infixr";
		{ Infix  }		| "infix";

either x y { Either a b } <- x { a }, y { b };
either x y { Left  x }	: x { x };
           { Right y }	| y { y };

sepEndBy x s { [a] } <- x { a }, s;
sepEndBy x s { [] }	:;
             { xs }	| sepBy x s { xs }, s;

sepMaybeEndBy x s { [a] } <- x { a }, s;
sepMaybeEndBy x s { xs }	: sepBy x s { xs }, opt s;

some x { [a] } <- x { a };
some x { (x:xs) }		: x { x }, many x { xs };

bracket l x r { a } <- l, x { a }, r;
bracket l x r { x }	: l, x { x }, r;

}%

frown ts = failHere $ ParseFailMsg ("Parse Failure: " ++ show ts);

type HsName = (Maybe NameSpace, Q [Char]);

type PSt = Stream HsName;

type FixedT = ReaderT (FixMap HsName);

type Fixed = FixedT Id;

type PT m = ExcT ParseFailure (StateT PSt m);

type P = PT Id;

data ParseFailure = ParseFailMsg [Char];

fufM = runExcT >=> either (throw ∘ \ (_, v :: Q [Char]) -> ParseFailMsg ("unknown fixity: " ++ show v)) return

failHere = throw;

stlist :: (a -> b) -> ([a] -> b) -> [a] -> b;
stlist f g [x] = f x;
stlist f g  xs = g xs;

partitionDecls :: (Functor m, Monad m) => [Either ([a], b) (m (Either ([a1], b1) ([a2], b2)))] -> ([(a, b)], m ([(a1, b1)], [(a2, b2)]));
partitionDecls =
  let {
    go :: ([d] -> c) -> [Either ([a], b) d] -> ([(a, b)], c);
    go f = partitionEithers >>> x *** f;

    x = fmap (uncurry distribL) & join;
  } in go (sequence & fmap (go x));
