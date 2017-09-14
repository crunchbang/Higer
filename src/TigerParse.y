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

%%

program     : exp

dec         : tyDec
            | varDec
            | funDec

tyDec       : type id '=' ty

ty          : id
            | arrTy
            | recTy

arrTy       : array of id

recTy       : '{' fDeclist '}'

fDecList    : {- empty -}
            | fDecList ',' fieldDec
            | fieldDec

fieldDec    : id : id

funDec      : function id '(' fDecList ')' '=' exp
            | function id '(' fDecList ')' ':' id '=' exp

varDec      : var id ':=' exp
            | var id ':' id ':=' exp

lValue      : id
            | subscript
            | fieldExp

subscript   : lValue '[' exp ']'

fieldExp    : lValue '.' id

exp         : nil
            | lvalue
            | intLit
            | stringLit
            | seqExp
            | negation
            | callExp
            | infixExp
            | arrCreate
            | recCreate
            | assignment
            | ifThenElse
            | ifThen
            | whileExp
            | forExp
            | break
            | letExp

seqExp      : '(' expseq ')'

expseq      : {- empty -}
            | expseq ';' exp
            | exp

negation    : '-' exp

callExp     : id '(' arglist ')'

arglist     : {- empty -}
            | arglist ',' exp
            | exp

infixExp    : exp '*' exp
            | exp '/' exp
            | exp '+' exp
            | exp '-' exp
            | exp '=' exp
            | exp '<>' exp
            | exp '>' exp
            | exp '<' exp
            | exp '>=' exp
            | exp '<=' exp

arrCreate   : id '[' exp ']' of exp

recCreate   : id '{' recFList '}'

recFList    : fieldCreate
            | recFList ',' fieldCreate
            | {- empty -}

fieldCreate : id '=' exp

assignment  : lValue ':=' exp

ifThenElse  : if exp then exp else exp

ifThen      : if exp then exp

whileExp    : while exp do exp

forExp      : for id ':=' exp to exp do exp

letExp      : let declist in expseq end

declist     : declist dec
            | dec
