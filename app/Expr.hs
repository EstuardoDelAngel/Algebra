{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Expr ( Expr(..), Transformation, pow, funcs, d, dn, expand ) where

import Data.Ratio ( (%), numerator, denominator )
import Data.List ( intercalate )

data Expr = Add [Expr] | Mul Rational [Expr] | Exp Expr Expr | Constant Rational | Var String | Func String Expr-- add logs
type Transformation = Expr -> Expr

expBrackets :: Expr -> String
expBrackets (Add terms) = "(" ++ show (Add terms) ++ ")"
expBrackets (Mul coeff factors) = "(" ++ show (Mul coeff factors) ++ ")"
expBrackets x = show x

instance Show Expr where
    show (Add []) = "0"
    show (Add (term:terms)) = show term ++ concatMap ((\x -> case x of
        ('-':x) -> " - " ++ x
        x -> " + " ++ x) . show) terms
    show (Mul coeff []) = show (Constant coeff)
    show (Mul 1 factors) = intercalate "*" (map (\x -> case x of
        Add _ -> "(" ++ show x ++ ")"
        _ -> show x) factors)
    show (Mul (-1) factors) = "-" ++ show (Mul 1 factors)
    show (Mul coeff factors) = show (Constant coeff) ++ "*" ++ show (Mul 1 factors)
    show (Exp base (Constant expo))
        | expo == -1 = "1/" ++ show base
        | expo < 0 = "1/" ++ show (Exp base (Constant (-expo)))
    show (Exp base expo) = expBrackets base ++ "^" ++ expBrackets expo
    show (Constant i) = case (numerator i, denominator i) of
        (n, 1) -> show n
        (n, d) -> show n ++ "/" ++ show d
    show (Var v) = v
    show (Func f x) = f ++ "(" ++ show x ++ ")"

containSame :: Eq a => [a] -> [a] -> Bool -- this is kind of bad -- O(n^2) if it does return true
containSame [] [] = True
containSame _ [] = False
containSame [] _ = False
containSame (x:xs) ys = case break (== x) ys of
    (_, []) -> False
    (y', y's) -> containSame xs (y' ++ tail y's)

-- True if definitely equal
-- False is indeterminate as this doesn't apply transformations (identities, expansion, etc.)
instance Eq Expr where
    Add terms == Add terms' = containSame terms terms'
    Mul coeff factors == Mul coeff' factors' = coeff == coeff' && containSame factors factors'
    Exp base expo == Exp base' expo' = base == base' && expo == expo'
    Constant c == Constant c' = c == c'
    Var var == Var var' = var == var'
    Func f x == Func f' x' = f == f' && x == x'
    _ == _ = False

mulFac :: [Expr] -> [Expr] -> Expr -> [Expr]
mulFac res [] x = res ++ [x]
mulFac res (fac:rem) x = case fac * x of
    Mul _ _ -> mulFac (res ++ [fac]) rem x
    x' -> res ++ x':rem

addTerm :: [Expr] -> [Expr] -> Expr -> [Expr]
addTerm res [] x = res ++ [x]
addTerm res (term:rem) x = case term + x of
    Add _ -> addTerm (res ++ [term]) rem x
    Constant 0 -> res ++ rem
    x' -> res ++ x':rem

-- multiplcation and addition need some refactoring
instance Num Expr where
    Constant 0 + x = x
    x + Constant 0 = x

    Constant c + Constant c' = Constant (c + c')

    Add terms + Add terms' = foldr (+) (Add terms) terms'

    Add terms + x = Add (addTerm [] terms x)
    x + Add terms = Add terms + x

    Mul coeff factors + Mul coeff' factors' | containSame factors factors' = Mul (coeff + coeff') factors

    Mul coeff [factor] + x | factor == x = Mul (coeff + 1) [factor]
    x + Mul coeff [factor] = Mul coeff [factor] + x

    x + x' | x == x' = Constant 2 * x
    x + x' = Add [x, x']


    -- multiplication

    Constant 0 * _ = Constant 0
    _ * Constant 0 = Constant 0

    Constant 1 * x = x
    x * Constant 1 = x

    Constant c * Constant c' = Constant (c * c')

    Constant c * Mul coeff factors = Mul (c * coeff) factors
    Mul coeff factors * Constant c = Constant c * Mul coeff factors

    Constant c * x = Mul c [x]
    x * Constant c = Constant c * x

    Mul coeff factors * Mul coeff' factors' = foldr (*) (Mul (coeff * coeff') factors) factors'

    Mul coeff factors * x = Mul coeff (mulFac [] factors x)
    x * Mul coeff factors = Mul coeff factors * x

    Exp base expo * Exp base' expo' | base == base' = pow base (expo + expo')

    Exp base expo * x | base == x = pow base (expo + 1)
    x * Exp base expo = pow base expo * x

    x * x' | x == x' = pow x 2
    x * x' = Mul 1 [x, x']


    -- other functions required for num

    fromInteger x = Constant (toRational x)

    negate x = Constant (-1) * x

    abs (Constant x) = Constant (abs x)
    abs _ = error "tried to call abs on a non-constant expression" -- maybe num isn't the best fit ?

    signum (Constant x) = Constant (signum x)
    signum _ = error "tried to call signum on a non-constant expression"


instance Fractional Expr where
    fromRational x = Constant x

    recip (Constant x) = Constant (recip x)
    recip (Exp x y) = Exp x (negate y)
    recip x = Exp x (Constant (-1))


pow :: Expr -> Expr -> Expr
pow (Constant 0) (Constant 0) = Exp (Constant 0) (Constant 0) -- maybe handle this
pow x (Constant 0) = Constant 1
pow x (Constant 1) = x
pow (Constant 0) x = Constant 0
pow (Constant 1) x = Constant 1
pow (Constant x) (Constant y)
    | n /= 1 = pow (Constant (x ^^ n)) (Constant (1 % denominator y))
    where n = numerator y
pow (Exp x y) (Constant c) = pow x (y * Constant c)
pow x y = Exp x y


funcs :: [String]
funcs = ["sin", "cos", "tan",
    "cosec", "sec", "cot",
    "arcsin", "arccos", "arctan",
    "sinh", "cosh", "tanh",
    "ln"]

-- differentiation
d :: String -> Transformation

d x (Add us) = foldr (\u y -> y + d x u) (Constant 0) us -- change to foldr
d x (Mul coeff []) = Constant 0
d x (Mul coeff (u:vs)) = Constant coeff * (d x u * v + u * d x v) where v = Mul 1 vs -- product rule
d x (Exp u v) = pow u v * (Func "ln" u * d x v + (v * d x u) / u)

d _ (Constant _) = Constant 0
d x (Var v)
    | x == v = Constant 1
    | otherwise = Constant 0

d x (Func f u) = (case f of
    "sin" -> Func "cos" u
    "cos" -> negate (Func "sin" u)
    "tan" -> pow (Func "sec" u) (Constant 2)
    "cosec" -> negate (Func "cosec" u * Func "cot" u)
    "sec" -> Func "sec" u * Func "tan" u
    "cot" -> negate (pow (Func "cosec" u) (Constant 2))
    "arcsin" -> pow (Constant 1 - pow u (Constant 2)) (Constant (-1 % 2))
    "arccos" -> pow (Constant (-1) - pow u (Constant 2)) (Constant (-1 % 2))
    "arctan" -> recip (Constant 1 + pow u (Constant 2))
    "sinh" -> Func "cosh" u
    "cosh" -> Func "sinh" u
    "tanh" -> pow (Func "cosh" u) (Constant (-2))
    "ln" -> recip u
    _ -> error "something went wrong") * d x u -- chain rule

applyN :: (a -> a) -> Int -> a -> a
applyN f n x = iterate f x !! n

dn :: String -> Int -> Transformation
dn ex = applyN (d ex)


expandMul :: Expr -> Expr -> Expr
-- expandMul (Add terms) (Add terms') = foldr ((+) . expandMul (Add terms)) (Constant 0) terms'
expandMul (Add terms) (Add terms') = foldr ((+) . expandMul (Add terms)) (Constant 0) terms' -- Add (map (expandMul (Add terms)) terms')
expandMul (Add terms) x = foldr ((+) . (* x)) (Constant 0) terms
expandMul x (Add terms) = expandMul (Add terms) x
expandMul x x' = x * x'

expand :: Transformation
-- maybe put this stuff somewhere else, or don't allow empty lists at all
expand (Add []) = Constant 0
expand (Add [term]) = term
expand (Add terms) = Add (map expand terms)
expand (Mul coeff []) = Constant 0
expand (Mul coeff factors) = foldr expandMul (Constant coeff) factors -- do stuff
expand (Exp base (Constant i))
    | i < 0 = pow (expand (Exp base (Constant (-i)))) (Constant (-1))
    | otherwise = case (numerator i, denominator i) of
        (n, 1) -> foldr expandMul (Constant 1) (replicate (fromIntegral n) (expand base))
        (n, d) -> pow (expand (Exp base (Constant (fromIntegral n)))) (Constant (1 % d))
expand (Exp base expo) = pow (expand base) (expand expo)
expand (Func f x) = Func f (expand x)
expand x = x