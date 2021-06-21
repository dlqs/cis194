{- CIS 194 HW 11
   due Monday, 8 April
-}


module SExpr where

import AParser
import Control.Applicative
import Data.Char


-- (*>)       :: Applicative f => f a -> f b -> f b
-- (*>) fa fb = (\_ b -> b) <$> fa <*> fb
-- (*>) = liftA2 (\_ b -> b)

-- (*>) _ fb = fb
mapA       :: Applicative f => (a -> f b) -> ([a] -> f [b])
-- mapA fa (a:_) = fmap (\x -> [x]) (fa a) applied to only 1 a
mapA _ [] = pure []
mapA fa (a:as) = (:) <$> (fa a) <*> (mapA fa as)
sequenceA  :: Applicative f => [f a] -> f [a]
-- sequenceA fas = foldr (\fa fb -> (\a as -> (a:as)) <$> fa <*> fb) (pure []) fas
sequenceA = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA 0 _ = pure []
replicateA i fa = (:) <$> fa <*> (replicateA (i - 1) fa)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|>  pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (\o z -> o:z) <$> p <*> (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy $ isSpace

ident :: Parser String
ident = (:) <$> (satisfy $ isAlpha) <*> (zeroOrMore $ satisfy $ isAlphaNum)

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

igw :: Parser a -> Parser a
igw p = spaces *> p <* spaces

igparens :: Parser a -> Parser a
igparens p = (igw $ char '(' ) *> p <* (igw $ char ')')

parseAtom :: Parser Atom
parseAtom = parseNInteger <|> parseIIdent
  where parseNInteger = N <$> (igw posInt)
        parseIIdent = I <$> (igw ident)

parseSExp :: Parser SExpr
parseSExp = parseAAtom <|> parseCombSExp
  where parseAAtom = A <$> parseAtom
        parseCombSExp = Comb <$> ( igparens $ (zeroOrMore parseSExp) )
