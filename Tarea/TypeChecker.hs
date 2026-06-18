{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- CHEQUEO DE NOMBRES Y TIPOS -}
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
checkProg p = checkProgNames p
  
-- Chequeo de una expresión. IMPLEMENTAR Y NO TOCAR LA FIRMA
-- El comportamiento de la función se especifica en la letra de la Tarea.
checkExp :: Prog -> Exp -> CheckRes
checkExp _ _ = Ok

----------------------Chequeo de Nombres-------------------------------------------
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

------Combina patrón + cuerpo de la cláusula.------------------------------------------------------------
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
    -- si hay variables en el patron que habian sido declaradas DupVar
    -- y le concateno los errores detectados en el patron
   
   -- patErr = duplicados vars variables ++ patErrAux

checkClausesNames ::[Id] -> [Id] -> [Clause] -> [NameError]
checkClausesNames _ _ [] = []
checkClausesNames funciones variables (c:clausulas) = 
  checkClauseNames funciones variables c 
  ++ checkClausesNames funciones variables clausulas



-- Estructura sugerida por Chatty
-- El siguiente paso lógico por complejidad sería:

-- checkPatternNames PRONTO
-- checkExpNames PRONTO
-- checkClauseNames
-- checkStmtNames
-- checkStmtsNames
-- checkFunNames
-- checkProg

