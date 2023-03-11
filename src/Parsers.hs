{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Parsers where

import Control.Applicative
import Datatypes (Cpt (..))

data Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap func p1 = p1 >>= pure . func

instance Applicative Parser where
  pure a = Parser $ \str -> Just (a, str)
  p1 <*> p2 =
    p1
      >>= \func ->
        p2
          >>= \res -> pure (func res)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  p1 <|> p2 = Parser $ \str -> case runParser p1 str of
    Nothing -> runParser p2 str
    res -> res

instance Monad Parser where
  p1 >>= f = Parser $ \s ->
    runParser p1 s
      >>= \(x, xs) -> runParser (f x) xs

parseChar :: Char -> Parser Char
parseChar c = Parser $ \str -> case str of
  [] -> Nothing
  (x : xs) -> case x == c of
    True -> Just (c, xs)
    False -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser $ \_ -> Nothing
parseAnyChar (x : xs) = parseChar x <|> parseAnyChar xs

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = p1 <|> p2

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = p1 >>= \a -> p2 >>= \b -> return (a, b)

parseMany :: Parser a -> Parser [a]
parseMany p1 = Parser $ \str -> case runParser p1 str of
  Nothing -> Just ([], str)
  Just (x, xs) -> runParser (parseMany p1) xs >>= \(lst, ys) -> Just (x : lst, ys)

parseSome :: Parser a -> Parser [a]
parseSome p1 = (:) <$> p1 <*> parseMany p1

parseUInt :: Parser Int
parseUInt = read <$> (parseSome (parseAnyChar ['0' .. '9']))

parseInt :: Parser Int
parseInt = (*) <$> parseSign <*> parseUInt
  where
    parseSign = -1 <$ parseChar '-' <|> 1 <$ parseChar '+' <|> pure 1

parsePair :: Parser a -> Parser (a, a)
parsePair p1 = do
  _ <- parseChar '('
  _ <- parseMany (parseChar ' ')
  a <- p1
  _ <- parseSome (parseChar ' ')
  b <- p1
  _ <- parseMany (parseChar ' ')
  _ <- parseChar ')'
  return (a, b)

parseCptStart :: Parser Cpt
parseCptStart = do
  _ <- parseMany (parseAnyChar " \n\t")
  firstword <- parseNbr <|> parseSymbol
  cpts <-
    parseMany
      ( do
          _ <- parseMany (parseAnyChar " \n\t")
          cpt <- parseCpt
          _ <- parseMany (parseAnyChar " \n\t")
          return cpt
      )
  _ <- parseMany (parseAnyChar " \n\t")
  return (List (firstword : cpts))

parseCpt :: Parser Cpt
parseCpt = parseNbr <|> parseSymbol <|> parseList

parseNbr :: Parser Cpt
parseNbr = Nbr <$> parseInt

parseSymbol :: Parser Cpt
parseSymbol = Symbol <$> (parseSome (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "#+-*/?><%\\_=")))

parseList :: Parser Cpt
parseList = do
  _ <- parseChar '('
  _ <- parseMany (parseChar ' ')
  cpts <-
    parseMany
      ( do
          _ <- parseMany (parseChar ' ')
          cpt <- parseCpt
          _ <- parseMany (parseChar ' ')
          return cpt
      )
  _ <- parseMany (parseChar ' ')
  _ <- parseChar ')'
  return (List cpts)
