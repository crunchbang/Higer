module TigerParseHelper (Program(..),
                         Exp(..),
                         LValue(..),
                         InfixOp(..),
                         FieldCreate(..),
                         Decl(..),
                         Type(..),
                         FieldDecl(..),
                         AST,
                         startParse,
                         startCodeGen,
                         parseSeq,
                         MapEntry(..)
                        ) where

import Data.Map (Map)
import qualified Data.Map as Map

data MapEntry = SType Type
              | FType Type [Type]
              deriving (Show, Eq)

type Id = String
type SymTab = Map Id MapEntry 
type EvalType = MapEntry
type Code = String

class AST a where
        parse :: a -> SymTab -> (EvalType, SymTab)

class CG a where
        genCode :: a -> Code

startParse :: AST a => a -> (EvalType, SymTab)
startParse a = parse a (Map.empty)


startCodeGen :: CG a => a -> String
startCodeGen a = (genCode a) ++ mipsExit

------------------------------
data Program = Program Exp
             deriving (Eq)

instance Show Program where
        show (Program expr) = "(Program " ++ show expr ++ ")"

instance AST Program where
        parse (Program expr) symTab = parse expr symTab

mipsBegin = unlines [ ".text\n"
                    , "main:\n"
                    ]

instance CG Program where
        genCode (Program expr) = mipsBegin ++ (genCode expr)
------------------------------
data Exp = LExp LValue
         | NilValue
         | IntLiteral Int
         | StringLiteral String
         | SeqExp [Exp]
         | Negation Exp
         | CallExp    { 
                        callFunId :: Id, 
                        callFunArgs :: [Exp]                 
                      }
         | InfixExp   { 
                        infixLhs :: Exp, 
                        op :: InfixOp, 
                        infixRhs :: Exp       
                      }
         | ArrCreate  { 
                        arrType :: Type, 
                        size :: Exp, 
                        defVal :: Exp           
                      }
         | RecCreate  { 
                        recType :: Type,
                        recFields :: [FieldCreate]           
                      }
         | Assignment { 
                        assignmentLhs :: LValue,
                        assignmentRhs :: Exp         
                      }
         | IfThen     { 
                        ifCond :: Exp, 
                        thenExp :: Exp, 
                        elseExp :: Maybe Exp   
                      }
         | WhileExp   { 
                        whileCond :: Exp, 
                        whileBody :: Exp                    
                      }
         | ForExp     { 
                        forVar :: Id, 
                        low :: Exp, 
                        high :: Exp, 
                        forBody :: Exp 
                      }
         | Break
         | LetExp     { 
                        letDecl :: [Decl], 
                        letBody :: Exp                     
                      }
         deriving (Eq)




instance Show Exp where
        show (LExp lv) = "(LExp " ++ show lv ++ ")"
        show NilValue = "(NilValue)"
        show (IntLiteral i) = "(IntLiteral (" ++ show i ++ "))"
        show (StringLiteral s) = "(StringLiteral (" ++ show s ++ "))"
        show (SeqExp xs) = "(SeqExp " ++ listToArgs xs ++ ")"
        show (Negation e) = "(Negation " ++ show e ++ ")"
        show (CallExp { callFunId = cfid, callFunArgs = cfargs }) = "(CallExp (" ++ show cfid ++ ") " ++ listToArgs cfargs ++ ")"
        show (InfixExp { infixLhs = il, op = oper, infixRhs = ir }) = "(InfixExp " ++ show il ++ " " ++ show oper ++ " " ++ show ir ++ ")"
        show (ArrCreate { arrType = at, size = sz, defVal = dv }) = "(ArrCreate " ++ show at ++ " " ++ show sz ++ " " ++ show dv ++ ")"
        show (RecCreate { recType = rt, recFields = rf }) = "(RecCreate " ++ show rt ++ " " ++ listToArgs rf ++ ")"
        show (Assignment { assignmentLhs = al, assignmentRhs = ar }) = "(Assignment " ++ show al ++ " " ++ show ar ++ ")"
        show (IfThen { ifCond = ic, thenExp = te, elseExp = ee }) = "(IfThen " ++ show ic ++ " " ++ show te ++ " " ++ show ee ++ ")"
        show (WhileExp { whileCond = wc, whileBody = wb }) = "(WhileExp " ++ show wc ++ " " ++ show wb ++ ")"
        show (ForExp { forVar = fv, low = l, high = h, forBody = fb }) = "(ForExp (" ++ show fv ++ ") " ++ show l ++ " " ++ show h ++ " " ++ show fb ++ ")"
        show (Break) = "(Break)"
        show (LetExp { letDecl = ld, letBody = lb }) = "(LetExp (LetDecl " ++ listToArgs ld ++ ") (LetBody " ++ show lb ++ "))"


compareTypes :: (EvalType, SymTab) -> (EvalType, SymTab) -> (EvalType, SymTab)
compareTypes (type1, symTab1) (type2, _) = if type1 == type2 
                                                  then (type1, symTab1) 
                                                  else error ("[Semantic Error] Type mismatch: " ++ show type1 ++ " " ++ show type2)


checkCallExp :: Id -> [Type] -> SymTab -> (EvalType, SymTab)
checkCallExp funName callArgTypes symTab = if callArgTypes == argTypes 
                                              then (SType returnType, symTab) 
                                              else error ("[Semantic Error] Function Call Type mismatch: " ++ show callArgTypes ++ " " ++ show argTypes)
                                                      where Just (FType returnType argTypes) = Map.lookup funName symTab

exp2Types :: [Exp] -> SymTab -> [Type]
exp2Types expList symTab = case expList of
                             [] -> []
                             (e:es) -> eType : (exp2Types es symTab)
                                     where (SType eType, _) = parse e symTab


checkInfixExp :: (EvalType, SymTab) -> (EvalType, SymTab) -> InfixOp -> (EvalType, SymTab)
checkInfixExp (lhsType, symTab1) (rhsType, _) op 
  | lhsType /= rhsType = error ("[Semantic Error] Infix Type Error: " ++ show lhsType ++ " " ++ show rhsType)
  | op `elem` [Add, Sub, Mul, Div]  = if lhsType == (SType (Type "int")) 
                                         then (lhsType, symTab1) 
                                         else error ("[Semantic Error] Infix Type Error: " ++ show lhsType ++ " " ++ show rhsType)
  | op `elem` [GreaterThanEqual, GreaterThan, LessThanEqual, LessThan] = if lhsType == (SType (Type "int")) || lhsType == (SType (Type "string")) 
                                                                            then (lhsType, symTab1) 
                                                                            else error ("[Semantic Error] Infix Type Error: " ++ show lhsType ++ " " ++ show rhsType)
  | op `elem` [Equal, NotEqual] = (lhsType, symTab1)
  | otherwise = error "[Semantic Error] Infix Type Error(catchall)"


contains :: Eq a => [a] -> [a] -> Bool
contains [] y = True
contains (x:xs) y = elem x y && contains xs y

equals :: Eq a => [a] -> [a] -> Bool
equals x y = contains x y && contains y x

fieldDec2List :: [FieldDecl] -> [(Id, Type)]
fieldDec2List (f:fds) = (fId f, fType f) : fieldDec2List fds
fieldDec2List [] = []

fieldCreate2List :: [FieldCreate] -> SymTab -> [(Id, Type)]
fieldCreate2List (f:fcs) symTab = (fieldId, fieldTy) : fieldCreate2List fcs symTab
        where
                (FieldCreate fieldId _) = f
                (SType fieldTy, _) = parse f symTab
fieldCreate2List [] symTab = []


checkRecTypes :: Type -> [FieldCreate] -> SymTab -> (EvalType, SymTab)
checkRecTypes (Type recName) recFields symTab = if (equals declTypes recTypes) 
                                                   then (SType (Type recName), symTab) 
                                                   else error ("[Semantic error] Record Type Error:" ++ show recName ++ " " ++ show recFields 
                                                                ++ " " ++ show declTypes ++ " " ++ show recTypes)
                                                           where 
                                                                   (Just (SType (RecType fieldDecls))) = Map.lookup recName symTab
                                                                   declTypes = fieldDec2List fieldDecls
                                                                   recTypes  = fieldCreate2List recFields symTab


checkIfTypes :: Exp -> Exp -> Maybe Exp -> SymTab -> (EvalType, SymTab)
checkIfTypes ifCond thenExp maybeElse symTab = let (SType ifType, _) = parse ifCond symTab
                                                   (thenType, _) = parse thenExp symTab
                                               in case maybeElse of
                                                   Nothing        -> if ifType == (Type "int") 
                                                                        then (thenType, symTab) 
                                                                        else error ("[Semantic error] If Type Error: " ++ show thenType)
                                                   Just elseExp  -> if ifType == (Type "int") && (thenType == elseType) 
                                                                        then (thenType, symTab) 
                                                                        else error ("[Semantic error] If Type Error: " ++ show thenType ++ " " ++ show elseType)
                                                                             where (elseType, _) = parse elseExp symTab

checkWhileTypes :: Exp -> Exp -> SymTab -> (EvalType, SymTab)
checkWhileTypes whileCond whileBody symTab = if whileCondType == (Type "int") 
                                                then (whileBodyType, symTab) 
                                                else error "WhileCond error"
                                                        where
                                                                (SType whileCondType, _) = parse whileCond symTab
                                                                (whileBodyType, _) = parse whileBody symTab

checkForTypes :: Id -> Exp -> Exp -> Exp -> SymTab -> (EvalType, SymTab)
checkForTypes forVar low high forBody symTab = if lowType == (Type "int") && highType == (Type "int") 
                                                  then (bodyType, symTab) 
                                                  else error ("[Semantic error] For Type Error: " ++ show lowType ++ " " ++ show highType)
                                                          where
                                                                  (SType lowType, _) = parse low symTab
                                                                  (SType highType, _) = parse high symTab
                                                                  symTab' = Map.insert forVar (SType (Type "int")) symTab
                                                                  (bodyType, _) = parse forBody symTab'


checkLetTypes :: [Decl] -> Exp -> SymTab -> (EvalType, SymTab)
checkLetTypes letDecl letBody symTab = (letType, symTab)
        where
                (_, symTab') = parseSeq letDecl symTab
                (letType, _) = parse letBody symTab'

parseSeq :: AST a => [a] -> SymTab -> (EvalType, SymTab)
parseSeq [] symTab = error "[Semantic error] Empty Sequence"
parseSeq [e] symTab = parse e symTab
parseSeq (e:es) symTab = parseSeq es symTab'
                where (_, symTab') = parse e symTab

instance AST Exp where
        parse (LExp lvalue) symTab = parse lvalue symTab
        parse (NilValue) symTab = (SType (Type "NilValue"), symTab)
        parse (IntLiteral _) symTab = (SType (Type "int"), symTab)
        parse (StringLiteral _) symTab = (SType (Type "string"), symTab)
        parse (SeqExp es) symTab = parseSeq es symTab
        parse (Negation e) symTab = compareTypes (parse e symTab) (SType (Type "int"), symTab)
        parse (CallExp { callFunId = cfid, callFunArgs = cfargs }) symTab = checkCallExp cfid (exp2Types cfargs symTab) symTab 
        parse (InfixExp { infixLhs = il, op = op, infixRhs = ir }) symTab = checkInfixExp (parse il symTab) (parse ir symTab) op 
        parse (ArrCreate { arrType = at, size = sz, defVal = dv }) symTab = (SType at, symTab) 
        parse (RecCreate { recType = rt, recFields = rf }) symTab = checkRecTypes rt rf symTab
        parse (Assignment { assignmentLhs = al, assignmentRhs = ar }) symTab = compareTypes (parse al symTab) (parse ar symTab)
        parse (IfThen { ifCond = ic, thenExp = te, elseExp = ee }) symTab = checkIfTypes ic te ee symTab
        parse (WhileExp { whileCond = wc, whileBody = wb }) symTab = checkWhileTypes wc wb symTab
        parse (ForExp { forVar = fv, low = l, high = h, forBody = fb }) symTab = checkForTypes fv l h fb symTab
        parse (Break) symTab = (SType (Type "NilValue"), symTab)
        parse (LetExp { letDecl = ld, letBody = lb }) symTab = checkLetTypes ld lb symTab


conVar varName = varName ++ "_var"

mipsAlloc varName defVal = unlines [ ".data"
                                   , (conVar varName) ++ ": .word " ++ (show defVal)
                                   , ".text"
                                   ]

mipsLoad :: Exp -> String -> String
mipsLoad (LExp (LVar varName)) register = unlines [ "lw $" ++ register ++ ", " ++ (conVar varName) ]
mipsLoad (IntLiteral i) register = unlines [ "li $" ++ register ++ ", " ++ (show i) ]
mipsLoad _ _ = "# Unidentified exp encountered\n"

mipsStore varName register = unlines [ "sw $" ++ register ++ ", " ++ (conVar varName) ]

mipsExit = unlines [ "li $v0, 10"
                   , "syscall"
                   ]

mipsCond = unlines [ "blez $t0, else_b" ]
mipsCondExit = unlines [ "j exit_if" ]

mipsElseLabel = "else_b: \n"
mipsExitIfLabel = "exit_if: \n"


instance CG Exp where
        genCode item@(LExp (LVar varName)) = (mipsLoad item "t0")
        genCode (SeqExp xs) = unlines (map genCode xs)
        genCode (InfixExp { infixLhs = il, op = oper, infixRhs = ir }) = (mipsLoad il "t0") ++ (mipsLoad ir "t1") ++ (genCode oper)
        genCode (Assignment { assignmentLhs = (LVar lvarName), assignmentRhs = (IntLiteral i) }) = (mipsAlloc lvarName i)
        genCode (Assignment { assignmentLhs = (LVar lvarName), assignmentRhs = rAssign@(LExp _) }) = (mipsAlloc lvarName 0) ++ (mipsLoad rAssign "t2") ++ (mipsStore lvarName "t2")
        genCode (Assignment { assignmentLhs = (LVar lvarName), assignmentRhs = rAssign }) = (mipsAlloc lvarName 0) ++ (genCode rAssign) ++ (mipsStore lvarName "t7")
        genCode (IfThen { ifCond = ic, thenExp = te, elseExp = (Just e) }) = (genCode ic) ++ mipsCond ++ (genCode te) ++ mipsCondExit ++ mipsElseLabel ++ (genCode e) ++ mipsExitIfLabel
        genCode (IfThen { ifCond = ic, thenExp = te, elseExp = Nothing }) = (genCode ic) ++ mipsCond ++ (genCode te) ++ mipsCondExit ++ mipsExitIfLabel
        genCode (LetExp { letDecl = ld, letBody = lb }) = (unlines (map genCode ld)) ++ (genCode lb) 
        genCode _ = "# Unidentified exp encountered\n"

------------------------------

data LValue = LVar Id
            | LSubscript LValue Exp
            | LField LValue Id
            deriving (Eq)

instance Show LValue where
        show (LVar iden) = "(LVar (" ++ show iden ++ "))"
        show (LSubscript lval e) = "(LSubscript " ++ show lval ++ " " ++ show e ++ ")"
        show (LField lval iden) = "(LField " ++ show lval ++ " (" ++ show iden ++ "))"


getFieldType (f:fd) iden = if (fId f == iden) 
                              then fType f 
                              else getFieldType fd iden
getFieldType [] iden = error "Fieldtype mismatch"

instance AST LValue where
        parse (LVar iden) symTab = (entry, symTab)
                where (Just entry) = Map.lookup iden symTab
        parse (LSubscript lval e) symTab = if eTy == (Type "int") 
                                              then (parse lval symTab) 
                                              else error "LSubscript type error"
                                                      where (SType eTy, _) = parse e symTab
        parse (LField (LVar recName) iden) symTab = (SType idenTy, symTab)
                where 
                      (Just (SType (Type recTy))) = Map.lookup recName symTab
                      (Just (SType (RecType fd))) = Map.lookup recTy symTab
                      idenTy = getFieldType fd iden

instance CG LValue where
       -- genCode (LVar iden) = "(LVar (" ++ show iden ++ "))"
       -- genCode (LSubscript lval e) = "(LSubscript " ++ show lval ++ " " ++ show e ++ ")"
       -- genCode (LField lval iden) = "(LField " ++ show lval ++ " (" ++ show iden ++ "))"
        genCode _ = "# Unidentified exp encountered"
------------------------------
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

mipsAdd resReg opReg1 opReg2 = unlines ["add $" ++ resReg ++ ", $" ++ opReg1 ++ ", $" ++ opReg2 ]
mipsSub resReg opReg1 opReg2 = unlines ["sub $" ++ resReg ++ ", $" ++ opReg1 ++ ", $" ++ opReg2 ]

instance CG InfixOp where
        genCode Add = mipsAdd "t0" "t0" "t1"
        genCode Sub = mipsSub "t0" "t0" "t1"
        -- genCode Equal = mipsSub "t0" "t0" "t1" 
        -- genCode NotEqual = "(NotEqual)"
        genCode GreaterThan = mipsSub "t0" "t0" "t1"
        genCode LessThan = mipsSub "t0" "t1" "t0"
        --genCode GreaterThanEqual = "(GreaterThanEqual)"
        --genCode LessThanEqual = "(LessThanEqual)"
        genCode _ = "# Unidentified exp encountered"
------------------------------
data FieldCreate = FieldCreate Id Exp
                 deriving (Eq) 

instance Show FieldCreate where
        show (FieldCreate iden e) = "(FieldCreate (" ++ show iden ++ ") " ++ show e ++ ")"

instance CG FieldCreate where
        genCode _ = "# Unidentified exp encountered"
        

instance AST FieldCreate where
        parse (FieldCreate id e) symTab = (parse e symTab)
------------------------------
data Decl = TypeDec { typeId :: Id, ty :: Type }
          | VarDec  { varId :: Id, varType :: Maybe Type, value :: Exp }
          | FunDec  { declFunId :: Id, declFunArgs :: [FieldDecl], funRetType :: Maybe Type, funDef :: Exp }
          deriving (Eq)

instance CG Decl where
        genCode VarDec  { varId = vi, varType = vt, value = (IntLiteral i) } = (mipsAlloc vi i)
        genCode VarDec  { varId = vi, varType = vt, value = v } = (mipsAlloc vi 0) ++ (genCode v) ++ (mipsStore vi "t0")
        genCode _ = "# Unidentified exp encountered"

instance Show Decl where
        show TypeDec { typeId = ti, ty = t } = "(TypeDec (" ++ show ti ++ ") " ++ show t ++ ")"
        show VarDec  { varId = vi, varType = vt, value = v } = "(VarDec (" ++ show vi ++ ")  " ++ show vt ++ " " ++ show v ++ ")"
        show FunDec  { declFunId = dfid, declFunArgs = dfa, funRetType = frt, funDef = fd } = "(FunDec (" ++ show dfid ++ ") " ++ listToArgs dfa ++ " " ++ show frt ++ " " ++ show fd ++ ")"

isUnique :: Id -> SymTab -> Bool
isUnique id symTab = case (Map.lookup id symTab) of
                       Nothing -> True
                       _ -> False

checkVarDecTypes :: Id -> Maybe Type -> Exp -> SymTab -> (EvalType, SymTab)
checkVarDecTypes varId maybeVarType value symTab = let (SType vTy, _) = parse value symTab
                                                       symTab' = Map.insert varId (SType vTy) symTab
                                                    in case maybeVarType of
                                                           Nothing      -> if (isUnique varId symTab) 
                                                                              then (SType vTy, symTab') 
                                                                              else error "[Semantic error] Duplicate declaration"
                                                           Just varType -> if (isUnique varId symTab) && varType == vTy 
                                                                              then (SType vTy, symTab') 
                                                                               else error ("[Semantic error] VarDec Type Error:" ++ show varType ++ " " ++ show vTy)

fieldDec2Type :: [FieldDecl] -> [Type]
fieldDec2Type (fd:fds) = fType fd : fieldDec2Type fds
fieldDec2Type [] = []

updateSymTabWithFun :: [FieldDecl] -> SymTab -> SymTab
updateSymTabWithFun (fd:fds) symTab = updateSymTabWithFun fds symTab' 
        where symTab' = Map.insert (fId fd) (SType (fType fd)) symTab
updateSymTabWithFun [] symTab = symTab

checkFunDecTypes :: Id -> [FieldDecl] -> Maybe Type -> Exp -> SymTab -> (EvalType, SymTab) 
checkFunDecTypes funId funArgs maybeRetType funBody symTab = let argTypes = fieldDec2Type funArgs
                                                                 s' = updateSymTabWithFun funArgs symTab
                                                                 (_, _) = parse funBody s'
                                                              in case maybeRetType of
                                                                   Just retType -> if (isUnique funId symTab) 
                                                                                   then (SType retType, newSymTab) 
                                                                                   else error "[Semantic error] Duplicate Func Definition Error"
                                                                                           where newSymTab = Map.insert funId (FType retType argTypes) symTab
                                                                   Nothing -> if (isUnique funId symTab) 
                                                                                 then (SType (Type "NilValue"), newSymTab) 
                                                                                 else error "[Semantic error] Duplicate Func Definition Error"
                                                                                         where newSymTab = Map.insert funId (FType (Type "NilValue") argTypes) symTab
                

instance AST Decl where
        parse TypeDec { typeId = ti, ty = t } symTab = (SType t, Map.insert ti (SType t) symTab)
        parse VarDec  { varId = vi, varType = vt, value = v } symTab = checkVarDecTypes vi vt v symTab
        parse FunDec  { declFunId = dfid, declFunArgs = dfa, funRetType = frt, funDef = fd } symTab = checkFunDecTypes dfid dfa frt fd symTab
------------------------------
data Type = Type Id
          | ArrType Type
          | RecType [FieldDecl]
          deriving (Eq)

instance Show Type where
        show (Type iden) = "(Type (" ++ show iden ++ "))"
        show (ArrType iden) = "(ArrType " ++ show iden ++ ")"
        show (RecType fds) = "(RecType " ++ listToArgs fds ++ ")"

instance CG Type where
        genCode _ = "# Unidentified exp encountered"

------------------------------
data FieldDecl = FieldDecl { fId :: Id, fType :: Type }
               deriving (Eq)

instance Show FieldDecl where
        show FieldDecl { fId = fiden, fType = ft } = "(FieldDecl (" ++ show fiden ++ ") " ++ show ft ++ ")"

instance CG FieldDecl where
        genCode _ = "# Unidentified exp encountered"
------------------------------

listToArgs :: Show a => [a] -> String
listToArgs (x:xs) = (show x) ++ " " ++ (listToArgs xs)
listToArgs []     = ""

