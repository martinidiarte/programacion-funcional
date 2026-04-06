-- ghci p1ej1.hs para compilar
-- :r para recompilar cambios


----------------------Ejercicio 1 --------------------------------------------------------------------------------
--Defina una funcion sumsqrs que tome 3 numeros y retorne la suma de los cuadrados de los dos mayores
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


sumsqrs1 :: Int -> Int -> Int -> Int 
sumsqrs1 x y z = fst(mayor x y z )^2 + snd(mayor x y z)^2

----------------------Ejercicio 2 --------------------------------------------------------------------------------
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


----------------------Ejercicio 3 --------------------------------------------------------------------------------
--Defina and y or usando expresiones condicionales. Haga lo mismo utilizando pattern matching
andExp :: Bool -> Bool -> Bool
andExp a b = a == b

andPM :: Bool -> Bool -> Bool
andPM a b
    | not a && not b = True
    | a  && b = True
    | otherwise = False

----------------------Ejercicio 4 --------------------------------------------------------------------------------
--Defina al conectivo logico implicacion como un operador de tipo Bool.
implica :: Bool -> Bool -> Bool
implica a b
    | not a       = True
    | a  && b     = True
    | otherwise   = False

----------------------Ejercicio 5 --------------------------------------------------------------------------------
--Supongamos que representamos fechas a traves de una tripla de enteros
--que corresponden a dia, mes y año. Defina una funcion edad que dada
--dos fechas, una representando la fecha de nacimiento de una persona, y la
--otra representando la fecha actual, calcula la edad en años de la persona.  
  
type Fecha = (Dia, Mes, Anio)
type Dia   = Int
type Mes   = Int
type Anio  = Int

getAnio :: Fecha -> Int
getAnio (_,_,a) = a
getMes :: Fecha -> Int
getMes (_,m,_) = m
getDia :: Fecha -> Int
getDia (d,_,_) = d

edad :: Fecha -> Fecha -> Int
edad f1 f2   
    | getMes f1 == getMes f2 && getDia f1 <= getDia f2 = getAnio f2 - getAnio f1
    | getMes f1 < getMes f2 = getAnio f2 - getAnio f1 
    | otherwise  = getAnio f2 - getAnio f1 - 1

----------------------Ejercicio 6 -------------------------------------------------------------------------------
--Se desea procesar informacion relativa a estudiantes. Cada estudiante esta
--dado por su nombre (cadena de caracteres), CI (entero), ano de ingreso
--(entero) y lista de cursos aprobados. Cada curso est´a dado por el nombre del curso (cadena de caracteres), c´odigo del curso (entero) y nota de
--aprobacion (entero).
--(a) Represente la informacion de cada estudiante a traves de tuplas.
--(b) Escriba una funcion que dado un estudiante retorne su nombre y CI.
--(c) Escriba una funcion que dado un estudiante retorne su ano de ingreso.
--d) Escriba una funcion que dado un estudiante y una nota retorne una
--lista con los codigos de los cursos que aprobo con esa nota. (Sugerencia: use comprension de listas).
--(e) Escriba una funcion que dada una lista de estudiantes retorne una
--lista de pares (nombre, CI) de aquellos estudiantes ingresados en
--un determinado ano dado como parametro. (Sugerencia: use comprension de listas)

type Curso = (String, Int, Int) -- (NombreCurso, Codigo, Aprovacion)
type Cursos = [Curso] 
type Estudiante = (String, Int, Int, Cursos) -- (Nombre, CI, Anio_Ingreso, Cursos)


-----------------------Para Pruebas-------------------------
est1 :: Estudiante 
est1 = ("Martin",123,2020,[("Prog",101,10),("Mat",102,8)])
est2 :: Estudiante 
est2 = ("Pepe",342,2020,[("Prog",101,10),("Mat",102,8)])
est3 :: Estudiante 
est3 = ("Nicole",542,2021,[("Prog",101,10),("Mat",102,8)])
estudiantes :: [Estudiante]
estudiantes = [est1,est2,est3]
--------------------------------------------------------------

getInfoEst :: Estudiante -> (String, Int)
getInfoEst (nombre,ci,_,_) = (nombre, ci)

getAnioIngreso :: Estudiante -> Int
getAnioIngreso (_,_,anio,_) = anio

getCodigo :: Curso -> Int
getCodigo (_,c,_) = c

getCodigosCursos :: Estudiante -> Int -> [Int]
getCodigosCursos (_, _, _, cursos) nota =
  [ codigo | (_, codigo, notaCur) <- cursos, notaCur == nota ] --[ resultado | elemento <- lista, condicion ]

--significa:
--recorrer cursos
--desarmar cada curso en (nomCur, codigo, notaCur)
--quedarte solo con los que cumplen notaCur == nota
--devolver codigo

--[ resultado | elemento <- lista, condicion ]
--para cada elemento de la lista, si cumple la condición, devuelvo el resultado

getIngresadosAnio :: [Estudiante] -> Int -> [(String, Int)]
getIngresadosAnio estudiantes anio = [(nombre,ci) | (nombre,ci,a1,_) <- estudiantes, a1 == anio]

----------------------Ejercicio 7--------------------------------------------------------------------------------
--Rehaga el ejercicio anterior usando ahora tipos de datos algebraicos en lugar de tuplas.
data CursoAlg = Curs String Int Int
data EstudianteAlg = Est String Int Int [CursoAlg]


--Otra forma de hacerlo para usar los nombres de los parametros
--Usando record syntaxis
data EstudianteAlg2 = Est2 
  { nombre :: String
  , ci :: Int
  , anio :: Int
  , cursos :: [CursoAlg]
  }


getInfoEstAlg :: EstudianteAlg -> (String, Int)
getInfoEstAlg (Est nombre ci _ _) =  (nombre, ci)
--Usando la otra forma 
getInfoEstAlg2 :: EstudianteAlg2 -> (String, Int) 
getInfoEstAlg2 e = (nombre e, ci e) 

getAnioIngresoAlg :: EstudianteAlg -> Int
getAnioIngresoAlg (Est _ _ anio _) = anio

getCodigoAlg :: CursoAlg -> Int
getCodigoAlg (Curs _ c _) = c

getCodigosCursosAlg :: EstudianteAlg -> Int -> [Int]
getCodigosCursosAlg (Est _ _ _ cursos) nota =
  [ codigo | Curs _ codigo notaCur <- cursos, notaCur == nota ]  -- Constructor variables <- lista


--Para ejecutar-----------------------------------------------
estudiantesAlg :: [EstudianteAlg]
estudiantesAlg =
  [ Est "Martin" 123 2020 [Curs "Prog" 101 10, Curs "Mat" 102 8]
  , Est "Ana"    456 2021 [Curs "Prog" 101 9]
  , Est "Luis"   789 2020 [Curs "BD" 103 7]
  ]
--------------------------------------------------------------

----------------------Ejercicio 8--------------------------------------------------------------------------------
--Deseamos representar pares internamente ordenados, que son pares de
--numeros reales (r, s) tales que r <= s
--(a) Defina el tipo de los pares ordenados
--(b) Defina una funci´on que dado un par de reales cualesquiera retorna
--un par internamente ordenado.
--(c) Defina la operaci´on de suma de pares internamente ordenados, que
--suma las correspondientes componentes de dos pares retornando un
--nuevo par.
--(d) Defina la operaci´on de multiplicaci´on por un escalar, que dado un real
--y un par internamente ordenado multiplica la primera componente
--del par por el escalar. El resultado debe ser un par internamente ordenado. Si se pierde el orden se deben intercambiar las componentes.
data Par = Par Float Float
 deriving (Show, Eq)

crearPar :: Float -> Float -> Par
crearPar r s  
    | r <= s = Par r s
    |otherwise = Par s r

sumaPares :: Par -> Par -> Par
sumaPares (Par x1 y1) (Par x2 y2) = crearPar (x1+x2) (y1+y2) -- Constructor (exp1) (exp2) cuando tengo expresiones 
--Pattern matching funciona con:
--data → ✔️
--listas (:) → ✔️
--tuplas → ✔️
-- pero NO con funciones

productoPares :: Float -> Par -> Par
productoPares k (Par x y)  = crearPar (k*x) (k*y)

----------------------Ejercicio 9--------------------------------------------------------------------------------
--Todo numero entero x se puede descomponer de manera unica en terminos
--de dos numeros enteros y y z , tales que:
-- −5 < y <= 5
-- x = y + 10 × z.
--Defina una funcion que dado un entero x devuelve una tupla con los
--numeros y y z .

descomposicion :: Int -> (Int, Int)
descomposicion x = (mod x 10 , div x 10)


----------------------Ejercicio 10 --------------------------------------------------------------------------------
--Deseamos representar n´umeros racionales y operaciones sobre ellos. Los
--racionales son representados por pares de enteros cuya segunda componente es distinta de cero. Cada racional tiene infinitas representaciones,
--pero existe la llamada representaci´on can´onica en la que la segunda componente del par de enteros es mayor que cero y ambos enteros son primos
--entre si.
--(a) Defina el tipo racional
--(b) Defina una funci´on que dado un par de enteros, el segundo de los
--cuales es distinto de cero, retorne un racional en su representaci´on
--can´onica.
--(c) Defina las operaciones de suma, resta, multiplicaci´on, y negaci´on de
--racionales, e int2rac, que convierte un entero en un racional. Dichas
--operaciones deben devolver representaciones can´onicas como resultado.
--Nota: Puede usar la funci´on gcd (definida en el Prelude) la cual
--computa el m´aximo com´un divisor de dos n´umeros.

--data Racional = Rac 
--    {
--    parteEntera :: Int,
--   parteRacional :: Int 
--    }
--    deriving (Show, Eq)

data Racional = Rac Int Int
 deriving (Show, Eq)

repCanonica :: Int -> Int -> Racional
repCanonica x y 
    | x /= 0 && y > 0 = Rac (div x (gcd x y)) (div y (gcd x y))
    | x == 0 && y > 0 = Rac 0 0

sumaRac :: Racional -> Racional -> Racional
sumaRac (Rac n1 d1) (Rac n2 d2) = repCanonica (n1*d2 + n2*d1) (d1*d2)

restaRac :: Racional -> Racional -> Racional
restaRac (Rac n1 d1) (Rac n2 d2) = repCanonica (n1*d2 - n2*d1) (d1*d2)

prodRac :: Racional -> Racional -> Racional
prodRac (Rac n1 d1) (Rac n2 d2) = repCanonica (n1*n2) (d1*d2)

negRac :: Racional -> Racional
negRac (Rac n d) = repCanonica (-n) d

int2rac :: Int -> Racional
int2rac n = Rac n 1

----------------------Ejercicio 11 --------------------------------------------------------------------------------
--Dado el siguiente tipo para representar tri´angulos:
--data Triangulo = Equi Int | Iso Int Int | Esca Int Int Int
--Defina la funcion mkTriangulo que dados tres enteros positivos, que representan a los lados de un tri´angulo v´alido, retorna un valor de tipo
--Triangulo.

data Triangulo = Equi Int | Iso Int Int | Esca Int Int Int
 deriving (Show, Eq)

mkTriangulo :: Int -> Int -> Int -> Triangulo
mkTriangulo l1 l2 l3  
    | l1 == l2 && l2 == l3 = Equi l1
    | l1 == l2 = Iso l1 l3
    | l1 == l3 = Iso l1 l2
    | l2 == l3 = Iso l1 l2 
    | otherwise = Esca l1 l2 l3