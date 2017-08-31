{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  type                              { \s -> TYPE }
  int                               { \s -> TYPE_ID_int}
  string                            { \s -> TYPE_ID_string}
  var                               { \s -> VAR }
  function                          { \s -> FUNCTION }
  break                             { \s -> BREAK }
  of                                { \s -> OF }
  end                               { \s -> END }
  in                                { \s -> IN }
  nil                               { \s -> NIL }
  let                               { \s -> LET }
  do                                { \s -> DO }
  to                                { \s -> TO }
  for                               { \s -> FOR }
  while                             { \s -> WHILE }
  else                              { \s -> ELSE }
  then                              { \s -> THEN }
  if                                { \s -> IF }
  array                             { \s -> ARRAY }
  :\=                               { \s -> ASSIGN }
  \|                                { \s -> OR }
  &                                 { \s -> AND }
  \>\=                              { \s -> GEQ }
  \>                                { \s -> GTN }
  \<\=                              { \s -> LEQ }
  \<                                { \s -> LTN }
  \<\>                              { \s -> NEQ }
  \=                                { \s -> EQU }
  \/                                { \s -> DIVIDE }
  \*                                { \s -> MULTIPLY }
  \-                                { \s -> MINUS }
  \+                                { \s -> PLUS }
  \.                                { \s -> DOT }
  \}                                { \s -> RBRACE }
  \{                                { \s -> LBRACE }
  \[                                { \s -> LBRACK }
  \]                                { \s -> RBRACK }
  \)                                { \s -> RPAREN }
  \(                                { \s -> LPAREN }
  \;                                { \s -> SEMICOLON }
  :                                 { \s -> COLON }
  ","                               { \s -> COMMA }

  $white+                           ;
  "--".*                            ;
  $digit+                           { \s -> INT (read s) }
  $alpha [$alpha $digit \_ \’]*     { \s -> ID s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  TYPE |
  TYPE_ID_int |
  TYPE_ID_string |
  VAR |
  FUNCTION |
  BREAK |
  OF |
  END |
  NIL |
  LET |
  IN |
  DO |
  TO |
  FOR |
  WHILE |
  ELSE |
  THEN |
  IF |
  ARRAY |
  ASSIGN |
  OR |
  AND |
  GEQ |
  GTN |
  LEQ |
  LTN |
  NEQ |
  EQU |
  DIVIDE |
  MULTIPLY |
  MINUS |
  PLUS |
  DOT |
  RBRACE |
  LBRACE |
  LBRACK |
  RBRACK |
  RPAREN |
  LPAREN |
  SEMICOLON |
  COLON |
  COMMA |
  SYM Char |
  ID String |
  INT Int
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}