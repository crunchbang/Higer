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

program     : exp                                            { Program $1 }

dec         : tyDec                                          { $1 }
            | varDec                                         { $1 }
            | funDec                                         { $1 }

tyDec       : type id '=' ty                                 { TypeDec { typeId=$2 , ty=$4 } }

ty          : id                                             { Type $1 }
            | arrTy                                          { $1 }
            | recTy                                          { $1 }

arrTy       : array of id                                    { ArrType $3 }

recTy       : '{' fDeclist '}'                               { RecType $2 }

fDecList    : {- empty -}                                    { [] }
            | fieldDec ',' fDecList                          { $1 : $3 }
            | fieldDec                                       { [$1] }

fieldDec    : id ':' id                                      { FieldDecl { fId=$1, fType=$3 } }

funDec      : function id '(' fDecList ')' '=' exp           { FunDec { declFunId=$2, declFunArgs=$4, funRetType=Nothing, funDef=$7 } }
            | function id '(' fDecList ')' ':' id '=' exp    { FunDec { declFunId=$2, declFunArgs=$4, funRetType=$7, funDef=$9 } }

varDec      : var id ':=' exp                                { VarDec { varId=$2, varType=Nothing, value=$4 } }
            | var id ':' id ':=' exp                         { VarDec { varId=$2, varType=$4, value=$6 } }

lValue      : id                                             { LVar $1 }
            | subscript                                      { $1 }
            | fieldExp                                       { $1 }

subscript   : lValue '[' exp ']'                             { LSubscript $1 $3 }

fieldExp    : lValue '.' id                                  { LField $1 $3}

exp         : nil                                            { NilValue }
            | lValue                                         { LExp $1 }
            | intLit                                         { IntLiteral $1 }
            | stringLit                                      { StringLiteral $1 }
            | seqExp                                         { $1 }
            | negation                                       { $1 }
            | callExp                                        { $1 }
            | infixExp                                       { $1 }
            | arrCreate                                      { $1 }
            | recCreate                                      { $1 }
            | assignment                                     { $1 }
            | ifThenElse                                     { $1 }
            | ifThen                                         { $1 }
            | whileExp                                       { $1 }
            | forExp                                         { $1 }
            | break                                          { Break }
            | letExp                                         { $1 }

seqExp      : '(' expseq ')'                                 { if length($2) == 1 then head $2 else SeqExp (reverse $2) }

expseq      : {- empty -}                                    { [] }
            | expseq ';' exp                                 { $3 : $1 }
            | exp                                            { [$1] }

negation    : '-' exp                                        { Negation $1 }

callExp     : id '(' arglist ')'                             { CallExp { callFunId=$1, callFunArgs=(reverse $3) } }

arglist     : {- empty -}                                    { [] }
            | arglist ',' exp                                { $3 : $1 }
            | exp                                            { [$1] }

infixExp    : exp '*' exp                                    { InfixExp { infixLhs=$1, op=Mul, infixRhs=$3 } }
            | exp '/' exp                                    { InfixExp { infixLhs=$1, op=Div, infixRhs=$3 } }
            | exp '+' exp                                    { InfixExp { infixLhs=$1, op=Add, infixRhs=$3 } }
            | exp '-' exp                                    { InfixExp { infixLhs=$1, op=Sub, infixRhs=$3 } }
            | exp '=' exp                                    { InfixExp { infixLhs=$1, op=Equal, infixRhs=$3 } }
            | exp '<>' exp                                   { InfixExp { infixLhs=$1, op=NotEqual, infixRhs=$3 } }
            | exp '>' exp                                    { InfixExp { infixLhs=$1, op=GreaterThan, infixRhs=$3 } }
            | exp '<' exp                                    { InfixExp { infixLhs=$1, op=LessThan, infixRhs=$3 } }
            | exp '>=' exp                                   { InfixExp { infixLhs=$1, op=GreaterThanEqual, infixRhs=$3 } }
            | exp '<=' exp                                   { InfixExp { infixLhs=$1, op=LessThanEqual, infixRhs=$3 } }

arrCreate   : id '[' exp ']' of exp                          { ArrCreate { arrType=$1, size=$3, defVal=$6 } }

recCreate   : id '{' recFList '}'                            { RecCreate { recType=$1, recFields=$3 } }

recFList    : fieldCreate                                    { [$1] }
            | recFList ',' fieldCreate                       { $3 : $1 }
            | {- empty -}                                    { [] }

fieldCreate : id '=' exp                                     { $3 }

assignment  : lValue ':=' exp                                { Assignment { assignmentLhs=$1, assignmentRhs=$3 } }

ifThenElse  : if exp then exp else exp                       { IfThen { ifCond=$2, thenExp=$4, elseExp=(Just $6) } }

ifThen      : if exp then exp                                { IfThen { ifCond=$2, thenExp=$4, elseExp=Nothing } }

whileExp    : while exp do exp                               { WhileExp { whileCond=$2, whileBody=$4 } }

forExp      : for id ':=' exp to exp do exp                  { ForExp { forVar=$2, low=$4, high=$6, forBody=$6 } }

letExp      : let declist in expseq end                      { LetExp { letDecl=(reverse $2), letBody=(if length($4) == 1 then head $4 else SeqExp (reverse $4)) } }

declist     : declist dec                                    { $2: $1 }
            | dec                                            { [$1] }
