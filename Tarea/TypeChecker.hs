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


-- Chequeo de un programa. IMPLEMENTAR
-- El comportamiento de la función se especifica en la letra de la Tarea.
checkProg :: Prog -> CheckRes
checkProg p = checkProgNames p
  
-- Chequeo de una expresión. IMPLEMENTAR
-- El comportamiento de la función se especifica en la letra de la Tarea.
checkExp :: Prog -> Exp -> CheckRes
checkExp _ _ = Ok

----------------------Chequeo de Nombres-------------------------------------------
checkProgNames :: Prog -> CheckRes
checkProgNames p = if null errs then Ok else HasNameErrors errs
  where
    errs = checkFunNamesDup p ++ checkFuncionesNames [] p 

--Chequeo que no haya dos funciones con el mismo nombre en el programa (global) 
checkFunNamesDup :: Prog -> [NameError]
checkFunNamesDup = aux []
  where 
    aux _ [] = []
    aux vistos (Fun f _ _ _ : fs)
      | f `elem` vistos = DupFun f : aux vistos fs
      | otherwise = aux (f : vistos) fs

--------Chequeo conjunto de funciones----------------------------------------
--funciones son las funciones alcanzables hasta este momento
checkFuncionesNames :: [Id] -> Prog -> [NameError]
checkFuncionesNames _ [] = []
checkFuncionesNames funciones (f:fs) =  erroresNuevos ++ checkFuncionesNames funcionesNuevas fs
  where 
    (funcionesNuevas, erroresNuevos) = checkFunNames [] funciones f

--------Chequea una función completa ------------------------------------
--Primer [Id]: funciones padre.
--Segundo [Id]: funciones visibles.
--Fun = función a revisar
--Retorna funciones acumuladas
checkFunNames :: [Id] -> [Id] -> Fun -> ([Id], [NameError])
checkFunNames padres funciones (Fun nombre params insts exp)  = 
  (nombre : funciones  , errs ++ checkExpNames (nombre : padres) funciones vars exp)  
  where 
    (vars, errs) = checkStmtsNames (nombre : padres) funciones [params] insts

------Chequea una instrucción individual --------------------------------------------------------------------
--Solo la asignacion me genera la visibilidad de una variable
--Primer [Id]: funciones padre.
--Segundo [Id]: funciones visibles.
--Tercero [Id]: variables visibles.
checkStmtNames :: [Id] -> [Id] -> [Id] -> Stmt -> ([Id], [NameError])
checkStmtNames padres funciones variables (Assign id exp) 
  | id `notElem` variables = ([id],checkExpNames padres funciones variables exp)
  | otherwise = ([],checkExpNames padres funciones variables exp)
checkStmtNames padres funciones variables (While exp insts) = ([], nuevosErrs ++ checkExpNames padres funciones variables exp)
  where
    (_, nuevosErrs) = checkStmtsNames padres funciones variables insts
checkStmtNames padres funciones variables (If exp insts1 insts2) = ([], errsCond ++ errs1 ++ errs2)
  where
    (_, errs1) = checkStmtsNames padres funciones variables insts1
    (_, errs2) = checkStmtsNames padres funciones variables insts2
    errsCond = checkExpNames padres funciones variables exp
checkStmtNames padres funciones variables (Case exp clausulas) = 
  ([],checkExpNames padres funciones variables exp 
  ++ checkClausesNames padres funciones variables clausulas)

-----Mantiene actualizado el ambiente de variables----------------------------------------------
-- Parametros de entrada:
-- funciones padre
-- funciones visibles
-- variables visibles
-- conjuntos de instrucciones

-- Parametros de salida:
-- (nuevasVariablesVisibles, errores)
checkStmtsNames :: [Id] -> [Id] -> [Id] -> Stmts -> ([Id], [NameError])
checkStmtsNames _ _ variables [] = (variables, [])
checkStmtsNames padres funciones variables (i:insts) =(varsFinales, errs1 ++ errs2)
  where
    (varsNuevas, errs1) = checkStmtNames padres funciones variables i
    (varsFinales, errs2) = checkStmtsNames padres funciones (variables ++ varsNuevas) insts

------Detecta UndefVar y UndefFun.----------------------------------------------------------------------------
--checkExpNames recibe funciones variables expr y retorna lista de NameError
-- el caso Call es el que detecta funciones no declaradas.
--padres = funciones padre.
--funciones = funciones visibles.
--variables = variables visibles.
checkExpNames :: [Id] -> [Id] -> [Id] -> Exp -> [NameError]
checkExpNames _ _ _ (LitN _) = []
checkExpNames _ _ _ (LitB _) = []
checkExpNames _ _ _ Nil = [] 
checkExpNames padres funciones variables (Cons e1 e2) =  
    checkExpNames padres funciones variables e1 ++ checkExpNames padres funciones variables e2
checkExpNames padres funciones variables (Head e) = checkExpNames padres funciones variables e
checkExpNames padres funciones variables (Tail e) = checkExpNames padres funciones variables e
checkExpNames padres funciones variables (Call id e) 
  | id `notElem` funciones || id `elem` padres = UndefFun id : checkExpNames padres funciones variables e
  | otherwise = checkExpNames padres funciones variables e
checkExpNames padres funciones variables (Var id) 
  | id `notElem` variables = [UndefVar id]
  | otherwise = []
checkExpNames padres funciones variables (BinOp op e1 e2) =  
  checkExpNames padres funciones variables e1 ++ checkExpNames padres funciones variables e2
checkExpNames padres funciones variables (UnOp op e) = checkExpNames padres funciones variables e

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
-- funciones padre
-- funciones visibles
-- variables visibles
-- y una clausula

--necesito obtener de el patron la lista de variables que usa
-- y si una de esas esta en variables -> DupVar
-- snd obtiene el segundo elemento de una tupla 
checkClauseNames :: [Id] -> [Id] -> [Id] -> Clause -> [NameError]
checkClauseNames padres funciones variables (Clause pat insts) = patErrAux ++ snd (checkStmtsNames padres funciones (variables ++ vars) insts)
  where
    -- obtengo variables introducidas por el patron y los errores detectados
    (vars, patErrAux) = checkPatternNames variables pat
    -- si hay variables en el patron que habian sido declaradas DupVar
    -- y le concateno los errores detectados en el patron
   
   -- patErr = duplicados vars variables ++ patErrAux

checkClausesNames :: [Id] -> [Id] -> [Id] -> [Clause] -> [NameError]
checkClausesNames _ _ _ [] = []
checkClausesNames padres funciones variables (c:clausulas) = 
  checkClauseNames padres funciones variables c 
  ++ checkClausesNames padres funciones variables clausulas



-- Estructura sugerida por Chatty
-- El siguiente paso lógico por complejidad sería:

-- checkPatternNames PRONTO
-- checkExpNames PRONTO
-- checkClauseNames
-- checkStmtNames
-- checkStmtsNames
-- checkFunNames
-- checkProg

