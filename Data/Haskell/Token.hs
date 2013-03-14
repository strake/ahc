module Data.Haskell.Token where

data Token = Syntactosem [Char]
           | VarName [Char] | VarSymbol [Char]
           | TypName [Char] | TypSymbol [Char]
           | LInteger Integer
           | LFloat Double
           | LChar Char
           | LChars [Char]
           | LParenth | RParenth
           | LBracket | RBracket
           | LBrace   | RBrace
           | SemiColon | Comma | BackTick
  deriving (Eq, Show);
