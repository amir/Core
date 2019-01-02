module Main where

import Language

main :: IO ()
main = putStrLn $ pprint preludeDefs
