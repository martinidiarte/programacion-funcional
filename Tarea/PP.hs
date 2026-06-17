{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- PRETTY-PRINTING -}
{- HLINT ignore "Use foldr" -}
module PP where

import AST

--Pretty-printing de Operadores PRONTO
ppBOp :: BOp -> String
ppBOp Add = "+"
ppBOp Sub = "-"
ppBOp Times = "*"
ppBOp Div = "/"
ppBOp Mod = "%"
ppBOp And = "&&"
ppBOp Or = "||"
ppBOp Equ = "=="
ppBOp Lt = "<"

ppUOp :: UOp -> String
ppUOp Minus = "-"
ppUOp Not ="!"

--Pretty-printing de Expresiones PRONTO
ppExp :: Exp -> String
ppExp (LitN e) = show e 
ppExp (LitB e) = show e 
ppExp Nil = "Nil" 
ppExp (Cons e1 e2) = "(" ++ "Cons " ++ ppExp e1 ++ " " ++ ppExp e2 ++ ")"
ppExp (Head e) = "(" ++ "head " ++ ppExp e ++ ")"
ppExp (Tail  e) = "(" ++ "tail " ++ ppExp e ++ ")"
ppExp (Call id e) = "(" ++ id ++ " " ++ ppExp e ++ ")"
ppExp (Var id) = id 
ppExp (BinOp bop e1 e2) = "(" ++ ppExp e1 ++ " " ++ ppBOp bop ++ " " ++ ppExp e2 ++ ")"
ppExp (UnOp uop e) = "(" ++ ppUOp uop ++ ppExp e ++ ")"

--Pretty-printing de patrones PRONTO
ppPattern :: Pattern -> String
ppPattern PNil = "Nil" 
ppPattern (PCons p1 p2) = "(" ++ "Cons " ++ ppPattern p1 ++ " " ++ ppPattern p2 ++ ")"
ppPattern (PLitN i) = show i
ppPattern (PLitB i) = show i
ppPattern (PVar i) = i

--Pretty-printing de Conjunto de Clausulas PRONTO
ppCClauses :: String -> [Clause] -> String
ppCClauses _ [] = ""
ppCClauses ind (c:xs) = ind ++ ppClause ind c ++ ppCClauses ind xs

--Pretty-printing de Clausulas PRONTO
ppClause :: String -> Clause -> String
ppClause ind (Clause p ins) = 
    ppPattern p ++ " -> {\n"
    ++ ppStmts (ind ++ "    ") ins
    ++ ind 
    ++ "};\n"

--Pretty-printing de conjunto de Instrucciones PRONTO
-- ind maneja la indentacion 
ppStmts :: String -> Stmts -> String
ppStmts _ [] = ""
ppStmts ind (i:xs) = ind ++ ppStmt ind i ++ ppStmts ind xs

----Pretty-printing de Instrucciones PRONTO
-- ind maneja la indentacion 
ppStmt :: String -> Stmt -> String
ppStmt ind (Assign id e) =  id ++ " := "  ++ ppExp e ++ ";\n"
----------- While ----------------------
ppStmt ind (While e i) = 
    "while " ++ ppExp e ++ " {\n" 
    ++ ppStmts (ind ++ "    ") i 
    ++ ind ++ "};\n"
------------ If ----------------------
ppStmt ind (If e i1 i2) = 
    "if " ++ ppExp e ++ " then {\n" 
    ++ ppStmts (ind ++ "    ") i1 
    ++ ind ++ "} else {\n"
    ++ ppStmts (ind ++ "    ") i2  ++ ind ++ "};\n"
----------- Case ----------------------
ppStmt ind (Case e clausulas) = 
    "case " ++ ppExp e ++ " of {\n"
    ++ ppCClauses (ind ++ "    ") clausulas
    ++ ind ++ "};\n"

-- Pretty-printing de una funcion PRONTO
ppFun :: Fun -> String
ppFun (Fun nombre param inst exp) = 
    "fun " ++ nombre ++ " "++ param ++" {\n"  
    ++ ppStmts "    " inst
    ++ "} " ++ ppExp exp ++ ";\n"

-- Pretty-printing de un programa PRONTO
-- El comportamiento de la función se especifica en la letra de la Tarea.
ppProg :: Prog -> String
ppProg [] = ""
ppProg [f] = ppFun f -- Caso de una funcion
ppProg (x:xs) = ppFun x ++ "\n" ++ ppProg xs

--Para encerrar un programa entre corchetes
ppProgAux :: Prog -> String
ppProgAux p = "{\n" ++ ppProg p ++ "}"


