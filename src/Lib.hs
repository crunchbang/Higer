module Lib
        (
         tokenizeFile,
         parseTokenizedFile,
         startSyntaxAnalysis,
         startCompilation
        ) where

import System.IO
import TigerLex
import TigerParse
import TigerParseHelper

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



startSyntaxAnalysis :: IO ()
startSyntaxAnalysis = do
        fileName <- getLine
        fileHandle <- openFile fileName ReadMode
        contents <- hGetContents fileHandle
        print (startParse (tigerParse (tokenize contents)))


startCompilation :: IO ()
--startCompilation = parseTokenizedFile
startCompilation = startSyntaxAnalysis
