{
module TigerLex (tokenize, Token) where
}

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  <0>           type                              { \s -> TYPE }
  <0>           int                               { \s -> TYPE_ID_INT}
  <0>           string                            { \s -> TYPE_ID_STRING}
  <0>           var                               { \s -> VAR }
  <0>           function                          { \s -> FUNCTION }
  <0>           break                             { \s -> BREAK }
  <0>           of                                { \s -> OF }
  <0>           end                               { \s -> END }
  <0>           in                                { \s -> IN }
  <0>           nil                               { \s -> NIL }
  <0>           let                               { \s -> LET }
  <0>           do                                { \s -> DO }
  <0>           to                                { \s -> TO }
  <0>           for                               { \s -> FOR }
  <0>           while                             { \s -> WHILE }
  <0>           else                              { \s -> ELSE }
  <0>           then                              { \s -> THEN }
  <0>           if                                { \s -> IF }
  <0>           array                             { \s -> ARRAY }
  <0>           \/\*.*\*\/                        ;
  <0>           :\=                               { \s -> ASSIGN }
  <0>           \|                                { \s -> OR }
  <0>           &                                 { \s -> AND }
  <0>           \>\=                              { \s -> GEQ }
  <0>           \>                                { \s -> GTN }
  <0>           \<\=                              { \s -> LEQ }
  <0>           \<                                { \s -> LTN }
  <0>           \<\>                              { \s -> NEQ }
  <0>           \=                                { \s -> EQU }
  <0>           \/                                { \s -> DIVIDE }
  <0>           \*                                { \s -> MULTIPLY }
  <0>           \-                                { \s -> MINUS }
  <0>           \+                                { \s -> PLUS }
  <0>           \.                                { \s -> DOT }
  <0>           \}                                { \s -> RBRACE }
  <0>           \{                                { \s -> LBRACE }
  <0>           \[                                { \s -> LBRACK }
  <0>           \]                                { \s -> RBRACK }
  <0>           \)                                { \s -> RPAREN }
  <0>           \(                                { \s -> LPAREN }
  <0>           \;                                { \s -> SEMICOLON }
  <0>           :                                 { \s -> COLON }
  <0>           ","                               { \s -> COMMA }  
  <0>           \"[^\"]*\"                        { \s -> STRING (read s)}
  <0>           $white+                           ;
  <0>           "--".*                            ;
  <0>           $digit+                           { \s -> INT (read s) }
  <0>           $alpha [$alpha $digit \_ \’]*     { \s -> ID s }

{

-- The token type:
data Token =
  TYPE              |
  TYPE_ID_INT       |
  TYPE_ID_STRING    |
  VAR               |
  FUNCTION          |
  BREAK             |
  OF                |
  END               |
  NIL               |
  LET               |
  IN                |
  DO                |
  TO                |
  FOR               |
  WHILE             |
  ELSE              |
  THEN              |
  IF                |
  ARRAY             |
  ASSIGN            |
  OR                |
  AND               |
  GEQ               |
  GTN               |
  LEQ               |
  LTN               |
  NEQ               |
  EQU               |
  DIVIDE            |
  MULTIPLY          |
  MINUS             |
  PLUS              |
  DOT               |
  RBRACE            |
  LBRACE            |
  LBRACK            |
  RBRACK            |
  RPAREN            |
  LPAREN            |
  SEMICOLON         |
  COLON             |
  COMMA             |
  STRING String     |
  SYM Char          |
  ID String         |
  INT Int
  deriving (Eq,Show)

tokenize :: String -> [Token]
tokenize contents = alexScanTokens contents

}
