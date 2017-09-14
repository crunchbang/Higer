module TigerParseHelper where

type Id = String

data Program = Program Exp
             deriving (Show, Eq)

data Exp = LExp LValue
         | NilValue
         | IntLiteral Int
         | StringLiteral String
         | SeqExp [Exp]
         | Negation Exp
         | CallExp { functionName :: Id, args :: [Exp] }
         | InfixExp { lhs :: Exp, op :: InfixOp, rhs :: Exp }
         | ArrCreate { arrType :: Id, size :: Exp, defVal :: Exp }
         | RecCreate { recType :: Id, fields :: [FieldCreate] }
         | Assignment { lhs :: LValue, rhs :: Exp }
         | IfThen { condition :: Exp, thenExp :: Exp, elseExp :: Maybe Exp }
         | WhileExp { condition :: Exp, body :: Exp }
         | ForExp { loopVar :: Id, low :: Exp, high :: Exp, body :: Exp }
         | Break
         | LetExp { decl :: [Decl], body :: [Exp] }
         deriving (Show, Eq)


data LValue = LVar Id
            | LSubscript LVaule Exp
            | LField LValue Id
            deriving (Show, Eq)

data InfixOp = Add
             | Sub
             | Mul
             | Div
             | Equal
             | NotEqual
             | GreaterThan
             | LessThan
             | GreaterThanEqual
             | LessThanEqual
             deriving (Show, Eq)

data FieldCreate = FieldCreate Id Exp
                 deriving (Show, Eq) 

data Decl = TypeDec { typeId :: Id, type :: Type }
          | VarDec  { varId :: Id, varType :: Maybe Id, value :: Exp }
          | FunDec  { funId :: Id, args :: [FieldDec], funRetType :: Maybe Id, funDef :: Exp }
          deriving (Show, Eq)

data Type = Type Id
          | ArrType Id
          | RecType FieldDec
          deriving (Show, Eq)

data FieldDec = FieldDec { fId :: Id, fType :: Id }
              deriving (Show, Eq)
