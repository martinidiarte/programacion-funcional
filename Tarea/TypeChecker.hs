{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- CHEQUEO DE NOMBRES Y TIPOS -}
{- HLINT ignore "Use foldr" -}
module TypeChecker where

import AST

-- Tipos
data Type = TInt | TBool | TList
  deriving Eq

-- Resultado de un Chequeo
data CheckRes = Ok
              | HasNameErrors [NameError]
              | HasTypeErrors [TypeError]

-- Errores de Nombres
data NameError
  = UndefVar Id
  | UndefFun Id
  | DupFun Id
  | DupVar Id

-- Errores de Tipos
data TypeError
  = CallArgType Id Type
  | BinOpWrongType BOp Type Type
  | UnOpWrongType UOp Type
  | CondNotBool Type
  | AssignTypeMismatch Id Type Type
  | PatMismatch Type Type
  | ConsExpType Type Type
  | HeadTailArg Type
  | WrongReturnType Id Type


-- Instancias de Show de tipos y resultados
instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show TList = "list"

instance Show NameError where
  showsPrec _ err = case err of
    UndefVar x ->
      showString "undefined variable: " . showString x

    UndefFun f ->
      showString "undefined function: " . showString f

    DupFun f ->
      showString "duplicated function: " . showString f

    DupVar v ->
      showString "duplicated variable: " . showString v

instance Show TypeError where
  showsPrec _ err = case err of
    CallArgType f t ->
      showString "invalid argument type in "
      . showString f
      . showString ": "
      . shows t

    BinOpWrongType bop t1 t2 ->
      showString "invalid argument type/s in operator "
      . shows bop
      . showString ": "
      . shows t1
      . showString ", "
      . shows t2

    UnOpWrongType uop t ->
      showString "invalid argument type in unary operator "
      . shows uop
      . showString ": "
      . shows t

    CondNotBool t ->
      showString "invalid condition type: "
      . shows t

    AssignTypeMismatch x t1 t2 ->
      showString "invalid assignment in "
      . showString x
      . showString ": expected "
      . shows t1
      . showString ", actual "
      . shows t2

    PatMismatch t1 t2 ->
      showString "invalid pattern: expected "
      . shows t1
      . showString ", actual "
      . shows t2

    ConsExpType t1 t2 ->
      showString "invalid argument type/s in Cons: "
      . shows t1
      . showString ", "
      . shows t2

    HeadTailArg t ->
      showString "invalid list argument type: "
      . shows t

    WrongReturnType f t ->
      showString "invalid return type in "
      . showString f
      . showString ": "
      . shows t

instance Show CheckRes where
  showsPrec _ Ok = showString "ok"
  showsPrec _ (HasNameErrors errs) = showLines errs
  showsPrec _ (HasTypeErrors errs) = showLines errs

showLines :: Show a => [a] -> ShowS
showLines =
  foldr1 (\x acc -> x . showChar '\n' . acc) . map shows

-- Chequeo de un programa. IMPLEMENTAR Y NO TOCAR LA FIRMA
-- El comportamiento de la función se especifica en la letra de la Tarea.

checkProg :: Prog -> CheckRes
checkProg p =
  case checkProgNames p of
    Ok -> checkProgTypes p
    res -> res
    
-- Chequeo de una expresión. IMPLEMENTAR Y NO TOCAR LA FIRMA
-- El comportamiento de la función se especifica en la letra de la Tarea.
checkExp :: Prog -> Exp -> CheckRes
checkExp p e =
  let funs = [n | Fun n _ _ _ <- p]

      nameErrs =
        checkExpNames funs [] e

      (_, typeErrs) =
        checkExpTypes [] e
  in case (nameErrs, typeErrs) of
       ([], []) -> Ok
       (ns, []) -> HasNameErrors ns
       ([], ts) -> HasTypeErrors ts
       (ns, ts) -> HasNameErrors ns

------------------------------------------------------------------------------------------------------------
---------------------- Chequeo de Nombres ------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
checkProgNames :: Prog -> CheckRes
checkProgNames p = if null errs then Ok else HasNameErrors errs
  where
    errs = checkFuncionesNames [] p 

--------Chequeo conjunto de funciones----------------------------------------
--funciones son las funciones alcanzables hasta este momento
checkFuncionesNames :: [Id] -> Prog -> [NameError]
checkFuncionesNames _ [] = []
checkFuncionesNames funciones (Fun nombre params insts exp:fs) 
  | nombre `elem` funciones = [DupFun nombre] ++ erroresNuevos ++ checkFuncionesNames funciones fs
  | otherwise = erroresNuevos ++ checkFuncionesNames (nombre : funciones) fs
  where 
    (funcionesNuevas, erroresNuevos) = checkFunNames funciones (Fun nombre params insts exp)

--------Chequea una función completa ------------------------------------
--[Id]: funciones visibles.
--Fun = función a revisar
--Retorna funciones acumuladas
checkFunNames :: [Id] -> Fun -> ([Id], [NameError])
checkFunNames funciones (Fun nombre params insts exp)  = 
  (nombre : funciones  , errs ++ checkExpNames funciones vars exp)  
  where 
    (vars, errs) = checkStmtsNames funciones [params] insts

------Chequea una instrucción individual --------------------------------------------------------------------
--Solo la asignacion me genera la visibilidad de una variable
--Primer [Id]: funciones visibles.
--Segundo [Id]: variables visibles.
checkStmtNames :: [Id] -> [Id] -> Stmt -> ([Id], [NameError])
checkStmtNames funciones variables (Assign id exp) 
  | id `notElem` variables = ([id],checkExpNames  funciones variables exp)
  | otherwise = ([],checkExpNames  funciones variables exp)
checkStmtNames  funciones variables (While exp insts) = ([], nuevosErrs ++ checkExpNames  funciones variables exp)
  where
    (_, nuevosErrs) = checkStmtsNames  funciones variables insts
checkStmtNames  funciones variables (If exp insts1 insts2) = ([], errsCond ++ errs1 ++ errs2)
  where
    (_, errs1) = checkStmtsNames  funciones variables insts1
    (_, errs2) = checkStmtsNames  funciones variables insts2
    errsCond = checkExpNames  funciones variables exp
checkStmtNames  funciones variables (Case exp clausulas) = 
  ([],checkExpNames  funciones variables exp 
  ++ checkClausesNames  funciones variables clausulas)

------Chequea un conjunto de instrucciones --------------------------------------------------------------------
-----Mantiene actualizado el ambiente de variables----------------------------------------------
-- Parametros de entrada:
-- funciones visibles
-- variables visibles
-- conjuntos de instrucciones
-- Parametros de salida:
-- (nuevasVariablesVisibles, errores)
checkStmtsNames :: [Id] -> [Id] -> Stmts -> ([Id], [NameError])
checkStmtsNames _ variables [] = (variables, [])
checkStmtsNames funciones variables (i:insts) =(varsFinales, errs1 ++ errs2)
  where
    (varsNuevas, errs1) = checkStmtNames  funciones variables i
    (varsFinales, errs2) = checkStmtsNames  funciones (variables ++ varsNuevas) insts

------Chequea una expresion --------------------------------------------------------------------
------Detecta UndefVar y UndefFun.----------------------------------------------------------------------------
--checkExpNames recibe funciones variables expr y retorna lista de NameError
-- el caso Call es el que detecta funciones no declaradas.
--funciones = funciones visibles.
--variables = variables visibles.
checkExpNames :: [Id] -> [Id] -> Exp -> [NameError]
checkExpNames _ _ (LitN _) = []
checkExpNames _ _ (LitB _) = []
checkExpNames _ _ Nil = [] 
checkExpNames funciones variables (Cons e1 e2) =  
    checkExpNames funciones variables e1 ++ checkExpNames  funciones variables e2
checkExpNames funciones variables (Head e) = checkExpNames  funciones variables e
checkExpNames funciones variables (Tail e) = checkExpNames  funciones variables e
checkExpNames funciones variables (Call id e) 
  | id `notElem` funciones = UndefFun id : checkExpNames  funciones variables e
  | otherwise = checkExpNames  funciones variables e
checkExpNames funciones variables (Var id) 
  | id `notElem` variables = [UndefVar id]
  | otherwise = []
checkExpNames funciones variables (BinOp op e1 e2) =  
  checkExpNames funciones variables e1 ++ checkExpNames  funciones variables e2
checkExpNames funciones variables (UnOp op e) = checkExpNames  funciones variables e

------Chequea un patron --------------------------------------------------------------------
------Detecta DupVar en patrones y devuelve las variables introducidas.------------------------------------
-- (idsIntroducidos, erroresDuplicados)
checkPatternNames :: [Id] -> Pattern -> ([Id], [NameError])
checkPatternNames variables (PVar x)  
  | x `elem` variables = ([], [DupVar x])
  | otherwise = ([x],[])
checkPatternNames _ PNil = ([], [])
checkPatternNames _ (PLitN _) = ([], [])
checkPatternNames _ (PLitB _) = ([], [])
checkPatternNames variables (PCons p1 p2) = 
    (ids1 ++ ids2 , errs1 ++ errs2 ++ duplicados ids1 ids2)
  where
    (ids1, errs1) = checkPatternNames variables p1
    (ids2, errs2) = checkPatternNames variables p2

duplicados :: [Id] -> [Id] -> [NameError]
duplicados vars1 vars2 =
    [DupVar x | x <- vars2, x `elem` vars1]

------Chequea una clausula  --------------------------------------------------------------------
-- Parametros:
-- funciones visibles
-- variables visibles
-- y una clausula
--necesito obtener de el patron la lista de variables que usa
-- y si una de esas esta en variables -> DupVar
-- snd obtiene el segundo elemento de una tupla 
checkClauseNames ::[Id] -> [Id] -> Clause -> [NameError]
checkClauseNames funciones variables (Clause pat insts) = patErrAux ++ snd (checkStmtsNames  funciones (variables ++ vars) insts)
  where
    -- obtengo variables introducidas por el patron y los errores detectados
    (vars, patErrAux) = checkPatternNames variables pat

------Chequea un conjunto de clausulas --------------------------------------------------------------------
checkClausesNames ::[Id] -> [Id] -> [Clause] -> [NameError]
checkClausesNames _ _ [] = []
checkClausesNames funciones variables (c:clausulas) = 
  checkClauseNames funciones variables c 
  ++ checkClausesNames funciones variables clausulas

------------------------------------------------------------------------------------------------------------
---------------------- Chequeo de Tipos --------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

--Variables con su tipo
type Variables = [(Id, Type)]

checkProgTypes :: Prog -> CheckRes
checkProgTypes p = if null errs then Ok else HasTypeErrors errs
  where
    errs = checkFunsTypes p 

checkFunsTypes :: [Fun] -> [TypeError]
checkFunsTypes [] = []
checkFunsTypes (x:xs) = checkFunTypes x ++ checkFunsTypes xs

checkFunTypes :: Fun -> [TypeError]
checkFunTypes (Fun nombre params insts exp)  = errs
  where
    (vars, errs2) = checkStmtsTypes [(params, TList)] insts
    (t1, errs1) = checkExpTypes vars exp
    errs =  errs2 ++ errs1 ++
      if t1 == TList
        then []
      else [WrongReturnType nombre t1]

checkExpTypes :: Variables -> Exp -> (Type, [TypeError])
checkExpTypes _ (LitN _) = (TInt, [])
checkExpTypes _ (LitB _) = (TBool, [])
checkExpTypes _ Nil = (TList, [])
checkExpTypes variables (Head e) =
  let (t, errs) = checkExpTypes variables e
  in if t == TList
        then (TInt, errs)
        else (TInt, errs ++ [HeadTailArg t])
checkExpTypes variables (Tail e) =
  let (t, errs) = checkExpTypes variables e
  in if t == TList
        then (TList, errs)
        else (TList, errs ++ [HeadTailArg t])
checkExpTypes variables (Cons e1 e2) =
  let (t1, errs1) = checkExpTypes variables e1
      (t2, errs2) = checkExpTypes variables e2
      errs = errs1 ++ errs2
  in if t1 == TInt && t2 == TList
        then (TList, errs)
        else (TList, errs ++ [ConsExpType t1 t2])
checkExpTypes variables (Call id e) =
  let (t,errs) = checkExpTypes variables e
  in if t == TList
        then (TList, errs)
        else (TList, errs ++ [CallArgType id t])
checkExpTypes variables (Var x) = -- REVISAR ESTA 
  case lookup x variables of
    Just t  -> (t, [])

checkExpTypes variables (BinOp op e1 e2) = 
  let (t1, errs1) = checkExpTypes variables e1
      (t2, errs2) = checkExpTypes variables e2
      errs = errs1 ++ errs2
  in case op of
    Add ->
      if t1 == TInt && t2 == TInt
        then (TInt, errs)
        else (TInt, errs ++ [BinOpWrongType op t1 t2])
    Sub ->
      if t1 == TInt && t2 == TInt
        then (TInt, errs)
        else (TInt, errs ++ [BinOpWrongType op t1 t2])
    Times ->
      if t1 == TInt && t2 == TInt
        then (TInt, errs)
        else (TInt, errs ++ [BinOpWrongType op t1 t2])
    Div ->
      if t1 == TInt && t2 == TInt
        then (TInt, errs)
        else (TInt, errs ++ [BinOpWrongType op t1 t2])
    Mod ->
      if t1 == TInt && t2 == TInt
        then (TInt, errs)
        else (TInt, errs ++ [BinOpWrongType op t1 t2])
    Lt ->
      if t1 == TInt && t2 == TInt
        then (TBool, errs)
        else (TBool, errs ++ [BinOpWrongType op t1 t2])
    Equ ->
      if t1 == t2
        then (TBool, errs)
        else (TBool, errs ++ [BinOpWrongType op t1 t2])    
    And ->
      if t1 == TBool && t2 == TBool
        then (TBool, errs)
        else (TBool, errs ++ [BinOpWrongType op t1 t2])
    Or ->
      if t1 == TBool && t2 == TBool
        then (TBool, errs)
        else (TBool, errs ++ [BinOpWrongType op t1 t2])
checkExpTypes variables (UnOp op e) =
  let (t, errs) = checkExpTypes variables e
  in case op of
    Minus ->
      if t == TInt
        then (TInt, errs)
        else (TInt, errs ++ [UnOpWrongType op t])
    Not ->
      if t == TBool
        then (TBool, errs)
        else (TBool, errs ++ [UnOpWrongType op t])

checkClausesTypes :: Variables -> Type -> [Clause] -> (Variables, [TypeError])
checkClausesTypes vars _ [] = (vars, [])
checkClausesTypes vars tExp (c:cs) =
  let (_, errs1) = checkClauseTypes vars tExp c
      (_, errs2) = checkClausesTypes vars tExp cs
  in (vars, errs1 ++ errs2)

checkClauseTypes :: Variables -> Type -> Clause -> (Variables, [TypeError])
checkClauseTypes vars tExp (Clause pat insts) =
  let (_,varsPat, errsPat) = checkPatternTypes vars pat tExp
      (varsStmt, errsStmt) = checkStmtsTypes (vars ++ varsPat) insts
  in (varsStmt, errsPat ++ errsStmt)


--Nos falta intruducir este error ConsExpType Type Type cuando los patrones del cons no respetan sus tipos
checkPatternTypes :: Variables -> Pattern -> Type -> (Type, Variables, [TypeError])
checkPatternTypes vars PNil t = (TList, vars, if  t == TList then [] else [PatMismatch t TList])
checkPatternTypes vars (PLitN _) t = (TInt, vars, if  t == TInt then [] else [PatMismatch t TInt])
checkPatternTypes vars (PLitB _) t = (TBool, vars, if t == TBool then [] else [PatMismatch t TBool])
checkPatternTypes vars (PCons p1 p2) t =
  let (t1, vars1, errs1) = checkPatternTypes vars p1 TInt
      (t2, vars2, errs2) = checkPatternTypes vars1 p2 TList
      errsCons =
        if t1 == TInt && t2 == TList
           then []
           else [ConsExpType t1 t2]
      errsPat =
        if t == TList
           then []
           else [PatMismatch t TList]
  in (TList, vars2, errs2 ++ errsCons ++ errsPat) --Saque el errs1 de la concatenacion de errores: errs1 ++
checkPatternTypes vars (PVar x) t = (t, (x,t):vars, []) --Las variables introducidas por un patrón son nuevas.

checkStmtsTypes :: Variables -> Stmts -> (Variables, [TypeError])
checkStmtsTypes variables [] = (variables,[])
checkStmtsTypes variables (x:xs) = (v2, errs1 ++ errs2)
  where 
    (v1, errs1) = checkStmtTypes variables x 
    (v2, errs2) = checkStmtsTypes v1 xs
  
checkStmtTypes :: Variables -> Stmt -> (Variables, [TypeError])
checkStmtTypes variables (Assign id e) =
  let (te, errs) = checkExpTypes variables e
  in case lookup id variables of
       Nothing ->
         -- primera asignación
         ((id, te) : variables, errs)

       Just tid ->
         if tid == te
            then (variables, errs)
          else (variables,
            errs ++ [AssignTypeMismatch id tid te])     
checkStmtTypes variables (While e insts) =
  let (t1,errs1) = checkExpTypes variables e
      (vars,errs2) = checkStmtsTypes variables insts
      errs = errs1 ++ errs2
  in if t1 == TBool
      then (variables, errs)
      else (variables, errs1 ++ [CondNotBool t1] ++ errs2)
checkStmtTypes variables (If e ins1 ins2) =
  let (te,errse) = checkExpTypes variables e
      (vars1,errs1) = checkStmtsTypes variables ins1
      (vars2,errs2) = checkStmtsTypes variables ins2
  in if te == TBool
      then (variables, errse ++ errs1 ++ errs2)
      else (variables, errse ++ [CondNotBool te] ++ errs1 ++ errs2)
checkStmtTypes variables (Case e cls) = (variables, errse ++ errs1)
  where 
    (te,errse) = checkExpTypes variables e
    (vars, errs1) = checkClausesTypes variables te cls 

