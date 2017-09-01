# Higer (Tiger + Haskell)



### Setting up the project
* Install stack by following the instructions on the official [stack](https://docs.haskellstack.org/en/stable/README/) page. 
* Stack sets up a separate environment for the project which does not interfere with  the already installed GHC and libraries.
```
$>stack setup # downloads and installs all the dependencies

$>stack build # builds the project and links the executable

$># run the project; give the path to any tiger program to get the lex output

$>stack exec Higer-exe
programs/array1.tig
[LET,TYPE,ID "arrtype",EQU,ARRAY,OF,TYPE_ID_INT,VAR,ID "arr1",COLON,ID "arrtype",ASSIGN,ID "arrtype",LBRACK,INT 10,RBRACK,OF,INT 0,IN,ID "arr1",END]

$># or run it by echoing the file name and piping the output to stack exec

$>echo "programs/array1.tig"|stack exec Higer-exe
[LET,TYPE,ID "arrtype",EQU,ARRAY,OF,TYPE_ID_INT,VAR,ID "arr1",COLON,ID "arrtype",ASSIGN,ID "arrtype",LBRACK,INT 10,RBRACK,OF,INT 0,IN,ID "arr1",END]

$>stack test #to run the tests in test/Spec.hs; Can be extended
Higer-0.1.0.0: test (suite: Higer-test)

Lexer Test
  Testing t.tiger:                   OK
  Testing record1.tiger:             OK
  Testing recursive_type:            OK
  Testing array2.tiger:              OK
  Testing recursive_factorial.tiger: OK

All 5 tests passed (0.00s)
```

### What has been done so far
* Lexical Analysis using Alex


### Requirements
* [stack](https://docs.haskellstack.org/en/stable/README/)

