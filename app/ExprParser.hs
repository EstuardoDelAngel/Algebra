{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module ExprParser ( parseCmd ) where

import Expr ( d, dn, expand, funcs, pow, Expr(Func, Var), Transformation )

import Control.Applicative ( Alternative(..) )
import Data.Char ( isDigit, isSpace, isAlpha )

-- basic parsers derived from http://www.cs.nott.ac.uk/~pszgmh/Parsing.hs

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

instance Functor Parser where
    fmap f p = P (\x -> case parse p x of
        Nothing -> Nothing
        Just (res, rem) -> Just (f res, rem))

instance Applicative Parser where
    pure v = P (\x -> Just (v, x))
    p <*> q = P (\x -> case parse p x of
        Nothing -> Nothing
        Just (res, rem) -> parse (res <$> q) rem)

instance Monad Parser where
    p >>= q = P (\x -> case parse p x of
        Nothing -> Nothing
        Just (res, rem) -> parse (q res) rem)

instance Alternative Parser where
    empty = P (const Nothing)
    p <|> q = P (\x -> case parse p x of
        Nothing -> parse q x
        Just (res, rem) -> Just (res, rem))

-- basic parsers

sat :: (Char -> Bool) -> Parser Char
sat f = do
    x <- P (\x -> case x of
        "" -> Nothing
        (x:xs) -> Just (x, xs))
    if f x then return x else empty

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

char :: Char -> Parser Char
char x = sat (== x)

digit :: Parser Char
digit = sat isDigit

string :: String -> Parser String
string "" = return ""
string (x:xs) = do
    char x
    string xs
    return (x:xs)

ident :: Parser String
ident = some (sat isAlpha)

-- specific parsers

constant :: Parser Expr
constant = do
    n <- some digit
    return (fromInteger (read n))

var :: Parser Expr
var = Var <$> ident

func :: Parser Expr
func = do
    s <- do
        x <- ident
        if x `elem` funcs then return x else empty
    char '('
    e <- add
    char ')'
    return (Func s e)

bracket :: Parser Expr
bracket = do
    char '('
    e <- add
    char ')'
    return e

atom :: Parser Expr
atom = do
    space
    e <- func <|> var <|> constant <|> bracket
    space
    return e

expExpr :: Parser Expr
expExpr = do
        e <- atom
        char '^'
        pow e <$> expExpr
    <|> atom

neg :: Parser Expr
neg = do
        space
        char '-'
        negate <$> neg
    <|> expExpr

leftAssoc :: (Parser Expr -> Expr -> Parser Expr) -> Parser Expr -> Parser Expr
leftAssoc f subExpr = do
    e <- subExpr
    f subExpr e

mulRec :: Parser Expr -> Expr -> Parser Expr
mulRec subExpr e = do
        char '*'
        e' <- subExpr
        mulRec subExpr (e * e')
    <|> do
        char '/'
        e' <- subExpr
        mulRec subExpr (e / e')
    <|> return e

mul :: Parser Expr
mul = leftAssoc mulRec neg

addRec :: Parser Expr -> Expr -> Parser Expr
addRec subExpr e = do
        char '+'
        e' <- subExpr
        addRec subExpr (e + e')
    <|> do
        char '-'
        e' <- subExpr
        addRec subExpr (e - e')
    <|> return e

add :: Parser Expr
add = leftAssoc addRec mul

cmd :: Parser Expr
cmd = do
        space
        string "dy/d"
        var <- ident
        d var <$> cmd
    <|> do
        space
        char 'd'
        n <- some digit
        string "y/d"
        var <- ident
        string n
        dn var (read n) <$> cmd
    <|> do
        space
        string "expand"
        expand <$> cmd
    <|> do
        space
        add

parseCmd :: String -> Maybe Expr
parseCmd x = case parse cmd x of
    Just (out, "") -> Just out
    _ -> Nothing