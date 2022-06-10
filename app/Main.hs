module Main where

import ExprParser ( parseCmd )

import System.IO ( hFlush, stdout )

main :: IO ()
main = do
    hFlush stdout
    line <- getLine
    putStrLn $ case parseCmd line of
        Just out -> show out
        _ -> "invalid command"
    main


-- trig exact ratios
-- integer + rational powers
-- like terms
-- terms
-- exact logs
-- maybe convert all negs to -1 then call another function to return them ?
-- turn weird trig into normal trig ? hyperbolics ?
-- 1/(thing)^power
-- collect like terms
-- exact roots

-- expand, factor
-- integrate
-- evaluate
-- other stuff idk