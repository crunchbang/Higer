module Lib
        (
         tokenizeFile,
         parseTokenizedFile
        ) where

import System.IO
import TigerLex
import TigerParse

tokenizeFile :: IO ()
tokenizeFile = do
        fileName <- getLine
        fileHandle <- openFile fileName ReadMode
        contents <- hGetContents fileHandle
        print (tokenize contents)

parseTokenizedFile :: IO ()
parseTokenizedFile = do
        fileName <- getLine
        fileHandle <- openFile fileName ReadMode
        contents <- hGetContents fileHandle
        print (tigerParse (tokenize contents))


parseShit = do
        fileHandle <- openFile "record1.tig" ReadMode
        contents <- hGetContents fileHandle
        print (tokenize contents)
        print (tigerParse (tokenize contents))


