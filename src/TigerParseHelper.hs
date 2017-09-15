module TigerParseHelper (Program(..),
                         Exp(..),
                         LValue(..),
                         InfixOp(..),
                         FieldCreate(..),
                         Decl(..),
                         Type(..),
                         FieldDecl(..),
                        ) where

type Id = String

data Program = Program Exp
             deriving (Eq)

instance Show Program where
        show (Program e) = "(Program " ++ show e ++ ")"



data Exp = LExp LValue
         | NilValue
         | IntLiteral Int
         | StringLiteral String
         | SeqExp [Exp]
         | Negation Exp
         | CallExp { callFunId :: Id, callFunArgs :: [Exp] }
         | InfixExp { infixLhs :: Exp, op :: InfixOp, infixRhs :: Exp }
         | ArrCreate { arrType :: Type, size :: Exp, defVal :: Exp }
         | RecCreate { recType :: Type, recFields :: [FieldCreate] }
         | Assignment { assignmentLhs :: LValue, assignmentRhs :: Exp }
         | IfThen { ifCond :: Exp, thenExp :: Exp, elseExp :: Maybe Exp }
         | WhileExp { whileCond :: Exp, whileBody :: Exp }
         | ForExp { forVar :: Id, low :: Exp, high :: Exp, forBody :: Exp }
         | Break
         | LetExp { letDecl :: [Decl], letBody :: Exp }
         deriving (Eq)

instance Show Exp where
        show (LExp lv) = "(LExp " ++ show lv ++ ")"
        show NilValue = "(NilValue)"
        show (IntLiteral i) = "(IntLiteral (" ++ show i ++ "))"
        show (StringLiteral s) = "(StringLiteral (" ++ show s ++ "))"
        show (SeqExp xs) = "(SeqExp " ++ listToArgs xs ++ ")"
        show (Negation e) = "(Negation " ++ show e ++ ")"
        show (CallExp { callFunId = cfid, callFunArgs = cfargs }) = "(CallExp (" ++ show cfid ++ ") " ++ listToArgs cfargs ++ ")"
        show (InfixExp { infixLhs = il, op = op, infixRhs = ir }) = "(InfixExp " ++ show il ++ " " ++ show op ++ " " ++ show ir ++ ")"
        show (ArrCreate { arrType = at, size = sz, defVal = dv }) = "(ArrCreate " ++ show at ++ " " ++ show sz ++ " " ++ show dv ++ ")"
        show (RecCreate { recType = rt, recFields = rf }) = "(RecCreate " ++ show rt ++ " " ++ listToArgs rf ++ ")"
        show (Assignment { assignmentLhs = al, assignmentRhs = ar }) = "(Assignment " ++ show al ++ " " ++ show ar ++ ")"
        show (IfThen { ifCond = ic, thenExp = te, elseExp = ee }) = "(IfThen " ++ show ic ++ " " ++ show te ++ " " ++ show ee ++ ")"
        show (WhileExp { whileCond = wc, whileBody = wb }) = "(WhileExp " ++ show wc ++ " " ++ show wb ++ ")"
        show (ForExp { forVar = fv, low = l, high = h, forBody = fb }) = "(ForExp (" ++ show fv ++ ") " ++ show l ++ " " ++ show h ++ " " ++ show fb ++ ")"
        show (Break) = "(Break)"
        show (LetExp { letDecl = ld, letBody = lb }) = "(LetExp (LetDecl " ++ listToArgs ld ++ ") (LetBody " ++ show lb ++ "))"

data LValue = LVar Id
            | LSubscript LValue Exp
            | LField LValue Id
            deriving (Eq)

instance Show LValue where
        show (LVar iden) = "(LVar (" ++ show iden ++ "))"
        show (LSubscript lval e) = "(LSubscript " ++ show lval ++ " " ++ show e ++ ")"
        show (LField lval iden) = "(LField " ++ show lval ++ " (" ++ show iden ++ "))"

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
             deriving (Eq)

instance Show InfixOp where
        show Add = "(Add)"
        show Sub = "(Sub)"
        show Mul = "(Mul)"
        show Div = "(Div)"
        show Equal = "(Equal)"
        show NotEqual = "(NotEqual)"
        show GreaterThan = "(GreaterThan)"
        show LessThan = "(LessThan)"
        show GreaterThanEqual = "(GreaterThanEqual)"
        show LessThanEqual = "(LessThanEqual)"

data FieldCreate = FieldCreate Id Exp
                 deriving (Eq) 

instance Show FieldCreate where
        show (FieldCreate iden e) = "(FieldCreate (" ++ show iden ++ ") " ++ show e ++ ")"

data Decl = TypeDec { typeId :: Id, ty :: Type }
          | VarDec  { varId :: Id, varType :: Maybe Type, value :: Exp }
          | FunDec  { declFunId :: Id, declFunArgs :: [FieldDecl], funRetType :: Maybe Type, funDef :: Exp }
          deriving (Eq)

instance Show Decl where
        show TypeDec { typeId = ti, ty = t } = "(TypeDec (" ++ show ti ++ ") " ++ show t ++ ")"
        show VarDec  { varId = vi, varType = vt, value = v } = "(VarDec (" ++ show vi ++ ")  " ++ show vt ++ " " ++ show v ++ ")"
        show FunDec  { declFunId = dfid, declFunArgs = dfa, funRetType = frt, funDef = fd } = "(FunDec (" ++ show dfid ++ ") " ++ listToArgs dfa ++ " " ++ show frt ++ " " ++ show fd ++ ")"

data Type = Type Id
          | ArrType Type
          | RecType [FieldDecl]
          deriving (Eq)

instance Show Type where
        show (Type iden) = "(Type (" ++ show iden ++ "))"
        show (ArrType iden) = "(ArrType " ++ show iden ++ ")"
        show (RecType fds) = "(RecType " ++ listToArgs fds ++ ")"

data FieldDecl = FieldDecl { fId :: Id, fType :: Type }
               deriving (Eq)

instance Show FieldDecl where
        show FieldDecl { fId = fiden, fType = ft } = "(FieldDecl (" ++ show fiden ++ ") " ++ show ft ++ ")"

listToArgs :: Show a => [a] -> String
listToArgs (x:xs) = (show x) ++ " " ++ (listToArgs xs)
listToArgs []     = ""

-- Helper functions for displaying the tree
