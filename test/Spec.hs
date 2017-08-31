module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TigerLex
import Lib

main :: IO ()
main = do
        defaultMain (testGroup "Lexer Test" [t1Test])

t1Test :: TestTree
t1Test = testCase "Testing Prog Name" (do contents <- readFile "programs/t.tiger"
                                          assertEqual "Unexpected token" [LET,ID "x",IN,INT 23] (tokenize contents))

