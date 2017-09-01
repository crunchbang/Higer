module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TigerLex
import Lib

main :: IO ()
main = do
        defaultMain (testGroup "Lexer Test" [t1Test, record1Test, recursive_typeTest, array2Test, recursive_factorialTest])

t1Test :: TestTree
t1Test = testCase "Testing t.tiger" (do contents <- readFile "programs/t.tiger"
                                        assertEqual "Unexpected token" [LET,ID "x",IN,INT 23] (tokenize contents))
record1Test :: TestTree
record1Test = testCase "Testing record1.tiger" (do contents <- readFile "programs/record1.tig"
                                                   assertEqual "Unexpected token" [LET,TYPE,ID "rectype",EQU,LBRACE,ID "name",COLON,TYPE_ID_STRING,COMMA,ID "age",COLON,TYPE_ID_INT,RBRACE,VAR,ID "rec1",COLON,ID "rectype",ASSIGN,ID "rectype",LBRACE,ID "name",EQU,STRING "Nobody",COMMA,ID "age",EQU,INT 1000,RBRACE,IN,ID "rec1",DOT,ID "name",ASSIGN,STRING "Somebody",SEMICOLON,ID "rec1",END] (tokenize contents))
recursive_typeTest :: TestTree
recursive_typeTest = testCase "Testing recursive_type" (do contents <- readFile "programs/recursive_type.tig"
                                                           assertEqual "Unexpected token" [LET,TYPE,ID "intlist",EQU,LBRACE,ID "hd",COLON,TYPE_ID_INT,COMMA,ID "tl",COLON,ID "intlist",RBRACE,TYPE,ID "tree",EQU,LBRACE,ID "key",COLON,TYPE_ID_INT,COMMA,ID "children",COLON,ID "treelist",RBRACE,TYPE,ID "treelist",EQU,LBRACE,ID "hd",COLON,ID "tree",COMMA,ID "tl",COLON,ID "treelist",RBRACE,VAR,ID "lis",COLON,ID "intlist",ASSIGN,ID "intlist",LBRACE,ID "hd",EQU,INT 0,COMMA,ID "tl",EQU,NIL,RBRACE,IN,ID "lis",END] (tokenize contents))

array2Test :: TestTree
array2Test = testCase "Testing array2.tiger" (do contents <- readFile "programs/array2.tig"
                                                 assertEqual "Unexpected token" [LET,TYPE,ID "arrtype1",EQU,ARRAY,OF,TYPE_ID_INT,TYPE,ID "rectype1",EQU,LBRACE,ID "name",COLON,TYPE_ID_STRING,COMMA,ID "address",COLON,TYPE_ID_STRING,COMMA,ID "id",COLON,TYPE_ID_INT,COMMA,ID "age",COLON,TYPE_ID_INT,RBRACE,TYPE,ID "arrtype2",EQU,ARRAY,OF,ID "rectype1",TYPE,ID "rectype2",EQU,LBRACE,ID "name",COLON,TYPE_ID_STRING,COMMA,ID "dates",COLON,ID "arrtype1",RBRACE,TYPE,ID "arrtype3",EQU,ARRAY,OF,TYPE_ID_STRING,VAR,ID "arr1",ASSIGN,ID "arrtype1",LBRACK,INT 10,RBRACK,OF,INT 0,VAR,ID "arr2",ASSIGN,ID "arrtype2",LBRACK,INT 5,RBRACK,OF,ID "rectype1",LBRACE,ID "name",EQU,STRING "aname",COMMA,ID "address",EQU,STRING "somewhere",COMMA,ID "id",EQU,INT 0,COMMA,ID "age",EQU,INT 0,RBRACE,VAR,ID "arr3",COLON,ID "arrtype3",ASSIGN,ID "arrtype3",LBRACK,INT 100,RBRACK,OF,STRING "",VAR,ID "rec1",ASSIGN,ID "rectype1",LBRACE,ID "name",EQU,STRING "Kapoios",COMMA,ID "address",EQU,STRING "Kapou",COMMA,ID "id",EQU,INT 2432,COMMA,ID "age",EQU,INT 44,RBRACE,VAR,ID "rec2",ASSIGN,ID "rectype2",LBRACE,ID "name",EQU,STRING "Allos",COMMA,ID "dates",EQU,ID "arrtype1",LBRACK,INT 3,RBRACK,OF,INT 1900,RBRACE,IN,ID "arr1",LBRACK,INT 0,RBRACK,ASSIGN,INT 1,SEMICOLON,ID "arr1",LBRACK,INT 9,RBRACK,ASSIGN,INT 3,SEMICOLON,ID "arr2",LBRACK,INT 3,RBRACK,DOT,ID "name",ASSIGN,STRING "kati",SEMICOLON,ID "arr2",LBRACK,INT 1,RBRACK,DOT,ID "age",ASSIGN,INT 23,SEMICOLON,ID "arr3",LBRACK,INT 34,RBRACK,ASSIGN,STRING "sfd",SEMICOLON,ID "rec1",DOT,ID "name",ASSIGN,STRING "sdf",SEMICOLON,ID "rec2",DOT,ID "dates",LBRACK,INT 0,RBRACK,ASSIGN,INT 2323,SEMICOLON,ID "rec2",DOT,ID "dates",LBRACK,INT 2,RBRACK,ASSIGN,INT 2323,END] (tokenize contents))

recursive_factorialTest :: TestTree
recursive_factorialTest = testCase "Testing recursive_factorial.tiger" (do contents <- readFile "programs/recursive_factorial.tig"
                                                                           assertEqual "Unexpected token" [LET,FUNCTION,ID "nfactor",LPAREN,ID "n",COLON,TYPE_ID_INT,RPAREN,COLON,TYPE_ID_INT,EQU,IF,ID "n",EQU,INT 0,THEN,INT 1,ELSE,ID "n",MULTIPLY,ID "nfactor",LPAREN,ID "n",MINUS,INT 1,RPAREN,IN,ID "nfactor",LPAREN,INT 10,RPAREN,END] (tokenize contents))
