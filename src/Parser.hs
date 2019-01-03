module Parser where

import Data.Char (isAlpha, isDigit)

type Token = String

type Parser a = [Token] -> [(a, [Token])]

isIdChar, isWhitespace :: Char -> Bool
isWhitespace c = c `elem` " \t\n"
isIdChar c = isAlpha c || isDigit c || (c == '_')

clex :: String -> [Token]
clex (c:cs) | isWhitespace c = clex cs
clex (c:cs) | isDigit c = num_token : clex rest_cs
  where
    num_token = c : takeWhile isDigit cs
    rest_cs = dropWhile isDigit cs
clex (c:cs) | isAlpha c = var_tok : clex rest_cs
  where
    var_tok = c : takeWhile isIdChar cs
    rest_cs = dropWhile isIdChar cs
clex (c:cs) = [c] : clex cs
clex [] = []

pLit :: String -> Parser String
pLit s (tok:toks)
  | s == tok = [(s, toks)]
  | otherwise = []
pLit s [] = []

pVar :: Parser String
pVar [] = []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks,
                                                      (v2, toks2) <- p2 toks1]

