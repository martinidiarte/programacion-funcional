{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- EVALUACIÓN DE EXPRESIONES -}
module Eval where

import AST
import State


-- Resultado de una evaluación
type EvalRes = Either RuntimeError Val

-- Errores en tiempo de ejecución
data RuntimeError
  = HeadOfEmptyList
  | TailOfEmptyList
  | DivisionByZero
  deriving Eq

instance Show RuntimeError where
  show HeadOfEmptyList = "head of empty list"
  show TailOfEmptyList = "tail of empty list"
  show DivisionByZero  = "division by zero"

  
-- Evalúa una expresión.
-- El comportamiento de la función se especifica en la letra de la Tarea.
evalExp :: Prog -> State -> Exp -> EvalRes

--Casos Base
evalExp _ _ (LitN n) =
  Right (ValInt n)

evalExp _ _ (LitB b) =
  Right (ValBool b)

evalExp _ _ Nil =
  Right (ValList [])

--Variables
evalExp _ st (Var x) =
  case get x st of
    Just v  -> Right v
    Nothing -> error ("Variable no encontrada: " ++ x)

--Cons
evalExp p st (Cons e1 e2) =
  case evalExp p st e1 of
    Left err -> Left err

    Right (ValInt n) ->
      case evalExp p st e2 of
        Left err -> Left err

        Right (ValList xs) ->
          Right (ValList (n:xs))

        _ -> error "tipo imposible"

    _ -> error "tipo imposible"

--Head
evalExp p st (Head e) =
  case evalExp p st e of
    Left err -> Left err

    Right (ValList []) ->
      Left HeadOfEmptyList

    Right (ValList (x:_)) ->
      Right (ValInt x)

    _ -> error "tipo imposible"

--Tail
evalExp p st (Tail e) =
  case evalExp p st e of
    Left err -> Left err

    Right (ValList []) ->
      Left TailOfEmptyList

    Right (ValList (_:xs)) ->
      Right (ValList xs)

    _ -> error "tipo imposible"

evalExp p st (Call f e) =
  case evalExp p st e of
    Left err -> Left err

    Right v ->
      case buscarFun f p of
        Nothing  -> error "funcion inexistente"
        Just fun -> evalFunc p st fun v
        
--Operadores Unarios
evalExp p st (UnOp Minus e) =
  case evalExp p st e of
    Left err -> Left err
    Right (ValInt n) -> Right (ValInt (-n))

evalExp p st (UnOp Not e) =
  case evalExp p st e of
    Left err -> Left err
    Right (ValBool b) -> Right (ValBool (not b))

--Operadores Binarios
evalExp p st (BinOp op e1 e2) =
  case evalExp p st e1 of
    Left err -> Left err

    Right v1 ->
      case evalExp p st e2 of
        Left err -> Left err

        Right v2 ->
          evalBinOp op v1 v2

evalBinOp :: BOp -> Val -> Val -> EvalRes
evalBinOp Add (ValInt x) (ValInt y) =
  Right (ValInt (x+y))

evalBinOp Sub (ValInt x) (ValInt y) =
  Right (ValInt (x-y))

evalBinOp Times (ValInt x) (ValInt y) =
  Right (ValInt (x*y))

evalBinOp Div (ValInt _) (ValInt 0) =
  Left DivisionByZero

evalBinOp Div (ValInt x) (ValInt y) =
  Right (ValInt (x `div` y))
  
evalBinOp Mod (ValInt _) (ValInt 0) =
  Left DivisionByZero

evalBinOp Mod (ValInt x) (ValInt y) =
  Right (ValInt (x `mod` y))

evalBinOp And (ValBool x) (ValBool y) =
  Right (ValBool (x && y))

evalBinOp Or (ValBool x) (ValBool y) =
  Right (ValBool (x || y))

evalBinOp Lt (ValInt x) (ValInt y) =
  Right (ValBool (x < y))

evalBinOp Equ v1 v2 =
  Right (ValBool (v1 == v2))

evalStmts :: Prog -> State -> Stmts -> Either RuntimeError State
evalStmts _ st [] = Right st

evalStmts p st (s:ss) =
  case evalStmt p st s of
    Left err  -> Left err
    Right st' -> evalStmts p st' ss

evalStmt :: Prog -> State -> Stmt -> Either RuntimeError State

evalStmt p st (Assign x e) =
  case evalExp p st e of
    Left err -> Left err
    Right v  -> Right (set x v st)

evalStmt p st (If e thenStmts elseStmts) =
  case evalExp p st e of
    Left err -> Left err

    Right (ValBool True) ->
      evalStmts p st thenStmts

    Right (ValBool False) ->
      evalStmts p st elseStmts

evalStmt p st w@(While e body) =
  case evalExp p st e of
    Left err -> Left err

    Right (ValBool False) ->
      Right st

    Right (ValBool True) ->
      case evalStmts p st body of
        Left err  -> Left err
        Right st' -> evalStmt p st' w

evalStmt p st (Case e clauses) =
  case evalExp p st e of
    Left err ->
      Left err

    Right v ->
      evalClauses p st v clauses

evalClauses :: Prog -> State -> Val -> [Clause]
            -> Either RuntimeError State

evalClauses _ st _ [] =
  Right st

evalClauses p st v (Clause pat body : cs) =
  case matchPattern pat v of

    Nothing ->
      evalClauses p st v cs

    Just vars ->
      let st' =
            foldr
              (\(x,val) acc -> new x val acc)
              (newFrame st)
              vars
      in case evalStmts p st' body of
           Left err ->
             Left err

           Right stFinal ->
             Right (dropFrame stFinal)

matchPattern :: Pattern -> Val -> Maybe [(Id, Val)]

matchPattern PNil (ValList []) =
  Just []

matchPattern (PLitN n) (ValInt m)
  | n == m = Just []
  | otherwise = Nothing

matchPattern (PLitB b) (ValBool c)
  | b == c = Just []
  | otherwise = Nothing

matchPattern (PVar x) v =
  Just [(x,v)]

matchPattern (PCons p1 p2) (ValList (x:xs)) =
  case matchPattern p1 (ValInt x) of
    Nothing -> Nothing

    Just vars1 ->
      case matchPattern p2 (ValList xs) of
        Nothing -> Nothing

        Just vars2 ->
          Just (vars1 ++ vars2)

matchPattern _ _ =
  Nothing


 
buscarFun :: Id -> Prog -> Maybe Fun
buscarFun _ [] = Nothing
buscarFun f (fun@(Fun nombre _ _ _):fs)
  | f == nombre = Just fun
  | otherwise   = buscarFun f fs

evalFunc :: Prog -> State -> Fun -> Val -> EvalRes
evalFunc p st (Fun _ param insts ret) arg =
  let st1 = newFrame st
      st2 = new param arg st1
  in case evalStmts p st2 insts of
       Left err ->
         Left err

       Right st3 ->
         evalExp p st3 ret
