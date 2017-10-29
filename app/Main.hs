module Main where

import Lib
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        let option = args !! 0
        startCompilation option
