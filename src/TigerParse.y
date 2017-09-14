{

module TigerParse where

import TigerLex(Token(..))

}

%name TigerParse
%tokentype { Token }
%error { parseError }

%right IN OF ELSE
%left '|'
%left '&'
%nonassoc '>' '<' '>=' '<=' '=' '<>'
%left '+' '-'
%left '*' '/'

%token

   type                              { TYPE }
   int                               { TYPE_ID_INT}
   string                            { TYPE_ID_STRING}
   var                               { VAR }
   function                          { FUNCTION }
   break                             { BREAK }
   of                                { OF }
   end                               { END }
   in                                { IN }
   nil                               { NIL }
   let                               { LET }
   do                                { DO }
   to                                { TO }
   for                               { FOR }
   while                             { WHILE }
   else                              { ELSE }
   then                              { THEN }
   if                                { IF }
   array                             { ARRAY }

   ':='                              { ASSIGN }
   '|'                               { OR }
   '&'                               { AND }
   '>='                              { GEQ }
   '>'                               { GTN }
   '<='                              { LEQ }
   '<'                               { LTN }
   '<>'                              { NEQ }
   '='                               { EQU }
   '/'                               { DIVIDE }
   '*'                               { MULTIPLY }
   '-'                               { MINUS }
   '+'                               { PLUS }
   '.'                               { DOT }
   '}'                               { RBRACE }
   '{'                               { LBRACE }
   '['                               { LBRACK }
   ']'                               { RBRACK }
   ')'                               { RPAREN }
   '('                               { LPAREN }
   ';'                               { SEMICOLON }
   ':'                               { COLON }
   ','                               { COMMA }  

   intLit                            { INT n }
   id                                { ID s }
   stringLit                         { STRING s }
