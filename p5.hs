module Practico5 where

import Data.List

--------------------------------------------------------------------------------
-- EJERCICIO 6
--------------------------------------------------------------------------------

-- Hamming

--El problema es:
--Hay que unir las tres listas.
--Hay que mantener el orden creciente.
--Hay que eliminar duplicados.

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs ys

hamming :: [Integer]
hamming =
    1 : merge (map (*2) hamming)
              (merge (map (*3) hamming)
                     (map (*5) hamming)) 

hammingTo :: Integer -> [Integer]
hammingTo = undefined

--------------------------------------------------------------------------------
-- EJERCICIO 7
--------------------------------------------------------------------------------

data Tarea
    = Limpiar
    | Cocinar
    | Fregar
    | LavarRopa
    | Comprar
    deriving (Show, Eq)

tareas :: [Tarea]
tareas = undefined

tareasPareja :: Int -> [Tarea] -> ([Tarea], [Tarea], [Tarea])
tareasPareja = undefined

planificar :: Int -> Int -> ([Tarea], [Tarea])
planificar = undefined

--------------------------------------------------------------------------------
-- EJERCICIO 8
--------------------------------------------------------------------------------

type Pos = (Int, Int)

data Juego =
    Juego Int
          Int
          Pos
          [Pos]
          Pos
    deriving Show

data Resultado
    = Gana
    | Pierde
    deriving Show

data ArbolJuego
    = Fin Resultado
    | Sigue ArbolJuego
            ArbolJuego
            ArbolJuego
            ArbolJuego
    deriving Show

data Mov
    = Ade
    | Atr
    | Izq
    | Der
    deriving Show

--------------------------------------------------------------------------------
-- 8.a
--------------------------------------------------------------------------------

iniciar ::
    Int ->
    Int ->
    Pos ->
    [Pos] ->
    Pos ->
    Maybe Juego

iniciar = undefined

--------------------------------------------------------------------------------
-- 8.b
--------------------------------------------------------------------------------

arbol :: Juego -> ArbolJuego
arbol = undefined

--------------------------------------------------------------------------------
-- 8.c
--------------------------------------------------------------------------------

mover :: Mov -> ArbolJuego -> ArbolJuego
mover = undefined

--------------------------------------------------------------------------------
-- 8.d
--------------------------------------------------------------------------------

jugar :: [Mov] -> ArbolJuego -> Maybe Resultado
jugar = undefined

--------------------------------------------------------------------------------
-- 8.e
--------------------------------------------------------------------------------

mejorJugada :: ArbolJuego -> [Mov]
mejorJugada = undefined
