module MonadicParser where

import Control.Monad

newtype Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item = Parser (\cs -> case cs of
                        "" -> []
                        (c:cs) -> [(c, cs)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) = f

instance Functor Parser where
  fmap f (Parser pa) = Parser (\x -> [(f a, cs) | (a, cs) <- pa x])

instance Applicative Parser where
  pure a = Parser (\cs -> [(a, cs)])
  -- f (a -> b) -> f a -> f b
  Parser fab <*> Parser fb = Parser $ (\x -> [(fa c, cs' ++ cs) | (c, cs) <- fb x, (fa, cs') <- fab x])


instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])

  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

trippleItem :: Parser (Char, Char, Char)
trippleItem = do
  a <- item
  b <- item
  c <- item
  return (a, b, c)
