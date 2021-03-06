{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
   fmap g (Parser f) = Parser $ (fmap (first g)) . f

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  -- Parser f <*> Parser g = Parser (\s -> f s >>= \(h, s') -> (return $ first h (g s')))
  -- Parser f <*> Parser g = Parser (>>= \(_, rs) -> g rs) . f
  Parser f <*> Parser h =
    Parser (\s -> case f s of
      Just (g, s') -> fmap (first g) $ h s'
      Nothing -> Nothing
    )

abParser :: Parser (Char, Char)
--abParser = Parser f
--  where
--    f ('a':'b':xs) = Just (('a', 'b'), xs)
--    f _ = Nothing

-- f(a -> b) f(a) => f(b)
-- (,) :: a -> b -> (a, b)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser ([Integer])
intPair = (\x _ z -> [x, z]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  -- p1 <|> p2 = (\x _ -> x) <$> p1 <*> p2
  --Parser f1 <|> Parser f2 = Parser (
  --  \s -> case f1 s of
  --    Just (a, s') -> Just (a, s')
  --    Nothing -> f2 s
  --                                 )
  Parser f1 <|> Parser f2 = Parser (\s -> f1 s <|> f2 s)

uppercaseChar :: Parser Char
uppercaseChar = satisfy (isUpper)

intOrUppercase :: Parser ()
intOrUppercase = ((\_ -> ()) <$> posInt) <|> ((\_ -> ()) <$> uppercaseChar)
-- doesn't work

-- runParser abParser "abcedf"
