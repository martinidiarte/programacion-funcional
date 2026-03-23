-- ghci p1ej1.hs para compilar
-- :r para recompilar cambios

--Auxiliares
--Ya hay una funcion max en haskell

-- Estructura para retornar por casos
-- funcion a b c
--  | caso1 = resultado1
--  | caso2 = resultado2
--  | otherwise = resultadoFinal

--Retorna los dos mayores de 3 numeros
mayor :: Int -> Int -> Int -> (Int,Int)
mayor x y z
        | x >= y && y >= z = (x,y) 
        | x >= y && z >= y = (x,z)
        | otherwise        = (y,z)   

--Ejercicio 1
--Defina una funcion sumsqrs que tome 3 numeros y retorne la suma de los cuadrados de los dos mayores
sumsqrs1 :: Int -> Int -> Int -> Int 
sumsqrs1 x y z = fst(mayor x y z )^2 + snd(mayor x y z)^2

--Defina una funcion analyze :: Int → Int → Int → Bool, que determina si tres enteros positivos son los lados de un tri´angulo.
--Propiedad de triangulos: 
--a + b > c 
--a + c > b
--b + c > a

analyze :: Int -> Int -> Int -> Bool
analyze a b c = 
    a + b > c && 
    a + c > b && 
    b + c > a


--Ejercicio 2
--Defina and y or usando expresiones condicionales. Haga lo mismo utilizando pattern matching
andExp :: Bool -> Bool -> Bool
andExp a b = a == b

andPM :: Bool -> Bool -> Bool
andPM a b
    | not a && not b = True
    | a  && b = True
    | otherwise = False

--Ejercicio 3
--Defina al conectivo logico implicacion como un operador de tipo Bool.
implica :: Bool -> Bool -> Bool
implica a b
    | not a       = True
    | a  && b     = True
    | otherwise   = False

--Ejercicio 4
--Supongamos que representamos fechas a traves de una tripla de enteros
--que corresponden a dia, mes y año. Defina una funcion edad que dada
--dos fechas, una representando la fecha de nacimiento de una persona, y la
--otra representando la fecha actual, calcula la edad en años de la persona.  
  
type fecha = (Dia, Mes, Anio)
type Dia   = Int
type Mes   = Int
type Anio  = Int

getAnio :: fecha -> Int
getAnio (_,_,a) = a
geMes :: fecha -> Int
getMes (_,_,a) = a
getDia :: fecha -> Int
getDia (_,_,a) = a

edad :: fecha -> fechas -> Int
edad f1 f2 = 
    | getAnio f2 - getAnio f1