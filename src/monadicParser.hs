module MonadicParser where

import Control.Monad
import Control.Applicative
import Data.Char



-- ###### a type of parser
newtype Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item = Parser (\cs -> case cs of
                        "" -> []
                        (c:cs) -> [(c, cs)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) = f


-- ###### a monad of parser
instance Functor Parser where
  fmap f (Parser pa) = Parser (\x -> [(f a, cs) | (a, cs) <- pa x])

instance Applicative Parser where
  pure a = Parser (\cs -> [(a, cs)])
  -- f (a -> b) -> f a -> f b
  Parser fab <*> Parser fb = Parser $ (\x -> [(fa c, cs) | (c, cs) <- fb x, (fa, _) <- fab x])


instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])

  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])


-- ###### a do notation for parser
trippleItem :: Parser (Char, Char, Char)
trippleItem = do
  a <- item
  b <- item
  c <- item
  return (a, b, c)

fab :: Parser (Char -> Int)
fab = return ord

-- example
-- parse (fab <*> item) "abc"
-- output: [(97, "bc")]

-- ######  choice combinators
instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance MonadPlus Parser where
  mzero = Parser (\cs -> [])
  mplus pa pb = Parser (\cs -> parse pa cs ++ parse pb cs)

(+++) :: Parser a -> Parser a -> Parser a
(+++) pa pb = Parser (\cs -> case parse (mplus pa pb) cs of
                         [] -> []
                         (x:xs) -> [x]
                     )

-- define a combinator with a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  case p c of
    True -> return c
    False -> mzero

-- create a specific char parser like 'd'
char :: Char -> Parser Char
char c = sat (c ==)

-- parse digit
digit :: Parser Char
digit = sat (isDigit)

lowercase :: Parser Char
lowercase = sat (isLower)

{-
-- example
λ:-) parse lowercase "aBc"
[('a',"Bc")]
λ:-) parse lowercase "Bbcc"
[]
λ:-) parse (lowercase <|> digit) "abc32Dd"
[('3',"abc32Dd")]
-}

-- ########## recursion combinators

-- parse a specific string
string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  char c
  string cs
  return (c:cs)
-- parse (string "abc") "abcdef" -> [("abc","def")]

-- parse repeated applications of a peser
many0 :: Parser a -> Parser [a]
many0 pa = many1 pa +++ return []

many1 :: Parser a -> Parser [a]
many1 pa = do
  c <- pa
  cs <- many0 pa
  return (c:cs)
-- parse (many0 $ char 'd') "ddddddabc" -> [("dddddd","abc")]
-- parse (many1 digit) "133def" -> [("133","def")]
-- parse (many1 digit) "def" -> []
-- parse (many0 digit) "def" -> [("","def")]

-- parse repeated applicataions of a parser p, seperated by applications of a parser sep, whose results are thrown away
sepby0 :: Parser a -> Parser b -> Parser [a]
sepby0 pa sep = sepby1 pa sep +++ mzero

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 pa sep = do
  a <- pa
  as <- many0 (many1 sep >> pa)
  return (a:as)
-- parse (sepby0 a b) "aaaa" -> [("a","aaa")]
-- parse (sepby0 a b) "abbbabbbaa" -> [("aaa","a")]
