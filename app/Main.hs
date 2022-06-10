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