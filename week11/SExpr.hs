{-# LANGUAGE ApplicativeDo #-}
{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do
    a <- p
    as <- zeroOrMore p
    pure $ a:as

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = do
    prefix <- oneOrMore $ satisfy isAlpha
    suffix <- zeroOrMore $ satisfy isAlphaNum
    pure $ prefix ++ suffix

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr = trim $ atomP <|> combP

atomP :: Parser SExpr
atomP = intgerP <|> identP

intgerP :: Parser SExpr
intgerP = A . N <$> posInt

identP :: Parser SExpr
identP = A . I <$> ident

combP :: Parser SExpr
combP = brace . trim $ Comb <$> zeroOrMore parseSExpr

trim :: Parser a -> Parser a
trim p = spaces *> p <*spaces

brace :: Parser a -> Parser a
brace p = char '(' *> p <* char ')'

main = mapM_ (print . runParser parseSExpr)
    [ "5"
    , "foo3"
    , "(bar (foo) 3 5 874)"
    , "(((lambda x (lambda y (plus x y))) 3) 5)"
    , "( lots of ( spaces in  ) this ( one  )  )" ]
