-- Thanks to Roman Cheplyaka for the regex-applicative library, which made this easier

{-# LANGUAGE OverloadedStrings #-}

module Haskell.Lex where

import Prelude hiding (elem, foldr, foldr1, sum, concat, fail);
import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monad hiding (fail);
import Control.Monad.Failure;
import Control.Monad.Reader hiding (fail);
import Control.Monad.State  hiding (fail);
import Control.Monad.Trans;
import Data.Bits;
import Data.Char;
import Data.Eq.Unicode;
import Data.Foldable;
import Data.Foldable.Unicode;
import Data.Haskell.Token;
import qualified Data.List as List;
import qualified Data.List.Unicode as List;
import Data.Located;
import Data.Maybe;
import Data.Monoid;
import Data.Tagged;
import Data.Text.Pos;
import Text.Regex.Applicative;
import Util;

newtype LexerT m a = LexerT (∀ b . (a -> [Char] -> m b) -> [Char] -> m b);

unLexerT :: LexerT m a -> (a -> [Char] -> m b) -> [Char] -> m b;
unLexerT (LexerT l) = l;

instance Functor m => Functor (LexerT m) where {
  fmap φ (LexerT x) = LexerT (x ∘ (∘ φ));
};

instance Monad m => Monad (LexerT m) where {
  return v = LexerT ($ v);
  LexerT l >>= f = LexerT (\ k -> l (flip unLexerT k ∘ f));
};

instance MonadTrans LexerT where {
  lift my = LexerT (\ k xs -> my >>= \ y -> k y xs);
  tmap f g (LexerT l) = LexerT (\ k -> f ∘ l (\ y -> g ∘ k y));
};

data ScanFailure = ScanFailMsg TextPos [Char];

instance Show ScanFailure where {
  show (ScanFailMsg p s) = "Scan Failure at " ++ show p ++ ": " ++ s;
};

failHere :: (Functor m, MonadFailure ScanFailure m, MonadState TextPos m) => [Char] -> m a;
failHere xs = get >>= \ p -> fail (ScanFailMsg p xs);

data Block = Pragma | Comment deriving Eq;

scan :: (Applicative m, Monad m) => [Char] -> FailureT ScanFailure m [Token];
scan = scanFrom 0;

scanFrom :: (Applicative m, Monad m) => TextPos -> [Char] -> FailureT ScanFailure m [Token];
scanFrom p = tmap (flip evalStateT p) lift ∘ unLexerT (whileJust scan1M return) (const ∘ return);

-- scan next token
-- yield Nothing at end of file
scan1M :: ∀ m . Monad m => LexerT (FailureT ScanFailure (StateT TextPos m)) (Maybe Token);
scan1M =
  LexerT $ \ (k :: Maybe Token -> [Char] -> FailureT ScanFailure (StateT TextPos m) b) ->
  let {
    scan' :: [Block] -> [Char] -> FailureT ScanFailure (StateT TextPos m) b;
    scan' bs = fromJust ∘ findLongestPrefix (concat <$> many reSpace) >>> (modify ∘ flip (foldr displ)) *=* return >=> \ ((), xs) ->
      case (bs, xs) of {
        ([], []) -> k Nothing ""; -- normal termination
        (bs, []) -> failHere $
                    "Unmatched " ++
                    List.intercalate ", " (fmap (\ b -> "{-" ++ case b of { Pragma -> "#"; _ -> ""; }) bs);
        (Comment:bs', xs) -> case findLongestPrefix ("{-" <|> "-}") xs of {
                               Just ("{-", xs) -> modify (+2) >> scan' (Comment:bs) xs;
                               Just ("-}", xs) -> modify (+2) >> scan'          bs' xs;
                               _ -> modify (displ (head xs)) >> scan' bs (tail xs);
                             };
        (Pragma :bs', xs) -> case findLongestPrefix ("{-" <|> "#-}") xs of {
                               Just ("{-",  xs) -> modify (+2) >> scan' (Comment:bs) xs;
                               Just ("#-}", xs) -> modify (+3) >> scan'          bs' xs;
                               _ -> modify (displ (head xs)) >> scan' bs (tail xs);
                             };
        (        [],  xs) -> case findLongestPrefix (Comment <$ "{-" <|>
                                                     Pragma  <$ "{-#") xs of {
                               Just (b, xs) -> modify (+ case b of {
                                                           Comment -> 2;
                                                           Pragma  -> 3;
                                                         }) >> scan' [b] xs;
                               _ -> case findLongestPrefix (withMatched reLexeme) xs of {
                                      Nothing -> failHere "Scan Failure";
                                      Just ((t, s), xs) -> modify (flip (foldr displ) s) >> k (Just t) xs;
                                    };
                             };
      };
  } in scan' [];

reLexeme :: RE Char Token;
reLexeme = foldr1 (<|>)
           [reSyntactosem,
            reVarName, reVarSymbol,
            reTypName, reTypSymbol,
            reLFloat, reLInteger, reLChar, reLChars,
            LParenth <$ sym '(', RParenth <$ sym ')',
            LBracket <$ sym '[', RBracket <$ sym ']',
            LBrace   <$ sym '{', RBrace   <$ sym '}',
            SemiColon <$ sym ';', Comma <$ sym ',', BackTick <$ sym '`'];

reSpace = some (psym isSpace) <|> reComment;

reComment = reLineComment;

reLineComment = dashes <++>
                (fmap (fromMaybe "") ∘ optional)
                (psym (not <$> (isSymbol <||> isPunctuation <||> (== '\n'))) *>
                 many (psym (/= '\n'))) <++> "\n";

dashes = "--" <++> many (sym '-');

reVarName, reTypName, reVarSymbol, reTypSymbol :: RE Char Token;
reVarName = VarName <$> reVarNameRaw;
reTypName = TypName <$> reTypNameRaw;
reVarSymbol = VarSymbol <$> reVarSymbolRaw;
reTypSymbol = TypSymbol <$> reTypSymbolRaw;

reVarNameRaw, reTypNameRaw, reVarSymbolRaw, reTypSymbolRaw :: RE Char [Char];
reVarNameRaw = psym isLower <:> many (psym isHsWordChar <|> sym '\'');
reTypNameRaw = psym isUpper <:> many (psym isHsWordChar <|> sym '\'');
reVarSymbolRaw = psym (isHsSymbol <&&> (/= ':')) <:> many (psym isHsSymbol);
reTypSymbolRaw = sym ':' <:> many (psym isHsSymbol);

reSyntactosem :: RE Char Token;
reSyntactosem = Syntactosem <$>
                foldr1 (<|>)
                [".", "..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>",
                 "case", "class", "data", "deriving", "foreign", "import", "in",
                 "infix", "infixl", "infixr", "instance", "let", "λ", "module",
                 "newtype", "of", "type", "where", "_"];

reLInteger :: RE Char Token;
reLInteger = LInteger <$> reInteger;

reLFloat :: RE Char Token;
reLFloat = fmap LFloat $
           reFloat 10 <|>
           (foldr1 (<|>) ∘ fmap (sym '0' *>))
           [psym (List.∈ "Bb") *> reFloat  2,
            psym (List.∈ "Oo") *> reFloat  8,
            psym (List.∈ "Dd") *> reFloat 10,
            psym (List.∈ "Xx") *> reFloat 16];

reFloat n = liftA2 (flip ($))
            (liftA2 (+) (fromIntegral <$> reNumber n) $
             sym '.' *> (foldr (\ m' m -> m/fromIntegral n + fromIntegral m') 0 <$> some (reNumeral n)))
            (fromMaybe id <$> optional reExponent) <|>
            liftA2 (flip ($)) (fromIntegral <$> reNumber n) reExponent;

reInteger :: RE Char Integer;
reInteger = reNumber 10 <|>
            (foldr1 (<|>) ∘ fmap (sym '0' *>))
            [psym (List.∈ "Bb") *> reNumber  2,
             psym (List.∈ "Oo") *> reNumber  8,
             psym (List.∈ "Dd") *> reNumber 10,
             psym (List.∈ "Xx") *> reNumber 16];

reExponent :: Num a => RE Char (a -> a);
reExponent = fmap (flip (^)) $ psym (List.∈ "Ee") *> (id <$ sym '+' <|> negate <$ sym '-') <*> reInteger;

reNumber :: Int -> RE Char Integer;
reNumber n = foldl' (\ m m' -> toInteger n*m + toInteger m') 0 <$> some (reNumeral n);

reNumeral :: Int -> RE Char Int;
reNumeral n | n <=  0 = empty
            | n <= 10 = (− fromEnum '0') ∘ fromEnum <$> psym (∈ take  n     ['0'..])
            | n <= 36 = (− fromEnum 'a') ∘ fromEnum <$> psym (∈ take (n-10) ['a'..]) <|>
                        (− fromEnum 'A') ∘ fromEnum <$> psym (∈ take (n-10) ['A'..]) <|>
                        reNumeral 10
            | otherwise = error ("Numeric radix " ++ show n ++ " too great");

reLChar :: RE Char Token;
reLChar = fmap LChar $
          sym '\'' *> (psym (isPrint <&&> not ∘ (List.∈ "\\'")) <|>
                       fromMaybe (error "Scan Failure: Null reEscape in character literal") <$>
                       reEscape) <* sym '\'';

reLChars :: RE Char Token;
reLChars = fmap LChars $
           sym '"' *> (catMaybes <$>
                       many (Just <$> psym (isPrint <&&> not ∘ (List.∈ "\\\"")) <|>
                             reEscape)) <* sym '"';

reEscape :: RE Char (Maybe Char);
reEscape = sym '\\' *>
         (Just <$>
          foldr1 (<|>)
          [charEscape, asciiEscape,
           toEnum ∘ fromIntegral <$>
           foldr1 (<|>)
           [sym 'b' *> reNumber  2,
            sym 'o' *> reNumber  8,
            sym 'd' *> reNumber 10,
            sym 'x' *> reNumber 16,
            reNumber 10]] <|>
          Nothing <$ sym '&')
  where {
    charEscape, asciiEscape :: RE Char Char;
    charEscape = foldr1 (<|>)
                 ['\a' <$ sym 'a',
                  '\b' <$ sym 'b',
                  '\f' <$ sym 'f',
                  '\v' <$ sym 'v',
                  '\t' <$ sym 't',
                  '\r' <$ sym 'r',
                  '\n' <$ sym 'n',
                  '"'  <$ sym '"',
                  '\'' <$ sym '\'',
                  '\\' <$ sym '\\'];
    asciiEscape = foldr1 (<|>)
                  ['\NUL' <$ "NUL",
                   '\SOH' <$ "SOH",
                   '\STX' <$ "STX",
                   '\ETX' <$ "ETX",
                   '\EOT' <$ "EOT",
                   '\ENQ' <$ "ENQ",
                   '\ACK' <$ "ACK",
                   '\BEL' <$ "BEL",
                   '\BS'  <$ "BS",
                   '\HT'  <$ "HT",
                   '\LF'  <$ "LF",
                   '\VT'  <$ "VT",
                   '\FF'  <$ "FF",
                   '\CR'  <$ "CR",
                   '\SO'  <$ "SO",
                   '\SI'  <$ "SI",
                   '\DLE' <$ "DLE",
                   '\DC1' <$ "DC1",
                   '\DC2' <$ "DC2",
                   '\DC3' <$ "DC3",
                   '\DC4' <$ "DC4",
                   '\NAK' <$ "NAK",
                   '\SYN' <$ "SYN",
                   '\ETB' <$ "ETB",
                   '\CAN' <$ "CAN",
                   '\EM'  <$ "EM",
                   '\SUB' <$ "SUB",
                   '\ESC' <$ "ESC",
                   '\FS'  <$ "FS",
                   '\GS'  <$ "GS",
                   '\RS'  <$ "RS",
                   '\US'  <$ "US",
                   '\SP'  <$ "SP",
                   '\DEL' <$ "DEL"];
  };

isHsSymbol, isHsWordChar :: Char -> Bool;
isHsSymbol = (isSymbol <||> isPunctuation) <&&> not ∘ (List.∈ "()[]{},;`'\"_");
isHsWordChar = isLetter <||> isNumber <||> (== '_');

infixr 5 <:>, <++>;

(<:>) :: Applicative p => p a -> p [a] -> p [a];
(<:>) = liftA2 (:);

(<++>) :: Applicative p => p [a] -> p [a] -> p [a];
(<++>) = liftA2 (++);

x − y = x - y;

displ :: Char -> TextPos -> TextPos;
displ '\r' (TextPos (m, _)) = TextPos (m,   0);
displ '\n' (TextPos (m, _)) = TextPos (m+1, 0);
displ '\b' (TextPos (m, n)) = TextPos (m, n-1);
displ   _  (TextPos (m, n)) = TextPos (m, n+1);
