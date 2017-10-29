module Lib
        (
         tokenizeFile,
         parseTokenizedFile,
         startSemanticAnalysis,
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



startSemanticAnalysis :: IO ()
startSemanticAnalysis = do
        fileName <- getLine
        fileHandle <- openFile fileName ReadMode
        contents <- hGetContents fileHandle
        print (startParse (tigerParse (tokenize contents)))


startCompilation :: String -> IO ()
startCompilation "semantic" = startSemanticAnalysis
startCompilation "syntax" = parseTokenizedFile
startCompilation _ = error "Unknown option"
