{

module TigerParse where

import TigerLex(Token(..),
                getString,
                getNum)
import TigerParseHelper (Program(..),
                         Exp(..),
                         LValue(..),
                         InfixOp(..),
                         FieldCreate(..),
                         Decl(..),
                         Type(..),
                         FieldDecl(..),
                        ) 

}

%name tigerParse
%tokentype { Token }
%error { parseError }

%nonassoc do assign
%right in of else
%left '|'
%left '&'
%nonassoc '>' '<' '>=' '<=' '=' '<>'
%left '+' '-'
%left '*' '/'
%left neg

%token

   type                              { TYPE }
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

tyDec       : type id '=' ty                                 { TypeDec { typeId=(getString $2) , ty=$4 } }

ty          : id                                             { Type (getString $1) }
            | arrTy                                          { $1 }
            | recTy                                          { $1 }

arrTy       : array of id                                    { ArrType (getString $3) }

recTy       : '{' fDecList '}'                               { RecType $2 }

fDecList    : {- empty -}                                    { [] }
            | fieldDec ',' fDecList                          { $1 : $3 }
            | fieldDec                                       { [$1] }

fieldDec    : id ':' id                                      { FieldDecl { fId=(getString $1), fType=(getString $3) } }

funDec      : function id '(' fDecList ')' '=' exp           { FunDec { declFunId=(getString $2), declFunArgs=$4, funRetType=Nothing, funDef=$7 } }
            | function id '(' fDecList ')' ':' id '=' exp    { FunDec { declFunId=(getString $2), declFunArgs=$4, funRetType=(Just (getString $7)), funDef=$9 } }

varDec      : var id ':=' exp                                { VarDec { varId=(getString $2), varType=Nothing, value=$4 } }
            | var id ':' id ':=' exp                         { VarDec { varId=(getString $2), varType=(Just (getString $4)), value=$6 } }

lValue      : id                                             { LVar (getString $1) }
            | subscript                                      { $1 }
            | fieldExp                                       { $1 }

subscript   : lValue '[' exp ']'                             { LSubscript $1 $3 }

fieldExp    : lValue '.' id                                  { LField $1 (getString $3)}

exp         : nil                                            { NilValue }
            | lValue                                         { LExp $1 }
            | intLit                                         { IntLiteral (getNum $1) }
            | stringLit                                      { StringLiteral (getString $1) }
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

seqExp      : '(' expseq ')'                                 { SeqExp (reverse $2) }

expseq      : {- empty -}                                    { [] }
            | expseq ';' exp                                 { $3 : $1 }
            | exp                                            { [$1] }

negation    : '-' exp                                        { InfixExp { infixLhs=(IntLiteral 0), op=Sub, infixRhs=$2 } }

callExp     : id '(' arglist ')'                             { CallExp { callFunId=(getString $1), callFunArgs=(reverse $3) } }

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

arrCreate   : id '[' exp ']' of exp                          { ArrCreate { arrType=(getString $1), size=$3, defVal=$6 } }

recCreate   : id '{' recFList '}'                            { RecCreate { recType=(getString $1), recFields=$3 } }

recFList    : fieldCreate                                    { [$1] }
            | recFList ',' fieldCreate                       { $3 : $1 }
            | {- empty -}                                    { [] }

fieldCreate : id '=' exp                                     { FieldCreate (getString $1) $3 }

assignment  : lValue ':=' exp                                { Assignment { assignmentLhs=$1, assignmentRhs=$3 } }

ifThenElse  : if exp then exp else exp                       { IfThen { ifCond=$2, thenExp=$4, elseExp=(Just $6) } }

ifThen      : if exp then exp                                { IfThen { ifCond=$2, thenExp=$4, elseExp=Nothing } }

whileExp    : while exp do exp                               { WhileExp { whileCond=$2, whileBody=$4 } }

forExp      : for id ':=' exp to exp do exp                  { ForExp { forVar=(getString $2), low=$4, high=$6, forBody=$8 } }

letExp      : let declist in expseq end                      { LetExp { letDecl=(reverse $2), letBody=(SeqExp (reverse $4)) } }

declist     : declist dec                                    { $2: $1 }
            | dec                                            { [$1] }

{

parseError :: [Token] -> a
parseError x  = error ("Parse error" ++ show x)
           
}
