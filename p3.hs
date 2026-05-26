import Control.Monad.Trans.Cont (reset)
----------------Ejercicio 1--------------------
-- Explique el tipo de las siguientes funciones:

-- (a)
-- min x y = if x < y then x else y

-- tipo Ord ya que se compara
min :: Ord a => a -> a -> a 
min x y = if x < y then x else y

-- (b)
-- paren x = "(" ++ show x ++ ")"

-- x puede ser de cualquier tipo mientras tenga la propiedad show
paren :: Int -> String
paren x = "(" ++ show x ++ ")"

----------------Ejercicio 2--------------------
-- Dada la siguiente definición de tipo:
-- data Semaforo = Verde | Amarillo | Rojo
-- ¿Qué falta para que sea posible hacer (show Verde)?

--Falta agregar deriving(show)
data Semaforo = Verde | Amarillo | Rojo
    deriving(Show)

----------------Ejercicio 3--------------------
-- Defina las siguientes funciones usando recursión explícita.

-- (a)
-- sumSqs :: Num a => [a] -> a
-- Suma los cuadrados de los elementos de una lista.

sumSqs :: Num a => [a] -> a
sumSqs [] = 0
sumSqs (x:xs) =  x^2 + sumSqs xs

-- (b)
-- elem :: Eq a => a -> [a] -> Bool
-- Determina si un elemento pertenece a una lista.

elem2 :: Eq a => a -> [a] -> Bool
elem2 x [] = False
elem2 x (y:xs) = x == y || elem2 x xs

-- (c)
-- elimDups :: Eq a => [a] -> [a]
-- Elimina los duplicados adyacentes de una lista.

elimDups :: Eq a => [a] -> [a]
elimDups [x] = [x]
elimDups (x:y:xs) 
    | x == y = elimDups (x:xs)  
    | otherwise = [x] ++ elimDups (y:xs)

-- (d)
-- split :: [a] -> ([a], [a])
-- Divide una lista en dos listas alternando elementos.

split :: [a] -> ([a], [a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:zs) = (x:xs, y:ys)
  where (xs, ys) = split zs

-- (e)
-- maxInd :: Ord a => [a] -> (a, Int)
-- Retorna el máximo y el índice de su primera ocurrencia.

--El máximo de (x:xs) es:
--  o bien x
--  o bien el máximo de xs
-- setea en 0 cuando encuentro un maximo ya que recorro desde el ultimo
-- entonces comienzo a contar cuantos elementos hay a la izq del maximo
-- guardando ese resultado en i
maxInd :: Ord a => [a] -> (a, Int)
maxInd [x] = (x, 0)
maxInd (x:xs) =
  let (m, i) = maxInd xs --se ejecuta esta linea hasta el caso base
  in if x > m
        then (x, 0)
        else (m, i + 1)

--Version con where
--para calcular maxInd (x:xs):
--primero sé que (m,i) es el resultado de maxInd xs
--y con eso decido qué devolver
maxInd2 :: Ord a => [a] -> (a, Int)
maxInd2 [x] = (x, 0)
maxInd2 (x:xs)
  | x > m     = (x, 0)
  | otherwise = (m, i + 1)
  where
    (m, i) = maxInd2 xs

-- (f)
-- merge :: Ord a => [a] -> [a] -> [a]
-- Mezcla dos listas ordenadas en una nueva lista ordenada.

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

----------------Ejercicio 4--------------------
-- Defina las siguientes funciones como foldr:

-- (a) sumSqs
--sumSqs :: Num a => [a] -> a
--sumSqs [] = 0
--sumSqs (x:xs) =  x^2 + sumSqs xs

--e = 0
--f = (^2)
sumSqsFoldr :: Num a => [a] -> a
sumSqsFoldr = foldr (\x acc -> x^2 + acc) 0 
-- en foldr es f x acc 
--    x es el elemento actual 
--    acc es el resultado de la recursión

-- (b) elimDups
--elimDups :: Eq a => [a] -> [a]
--elimDups [x] = [x]
--elimDups (x:y:xs) 
--    | x == y = elimDups (x:xs)  
--    | otherwise = [x] ++ elimDups (y:xs)

elimDupsFoldr :: Eq a => [a] -> [a]
elimDupsFoldr = foldr f []
    where 
        f x [] = [x]
        f x acc@(y:xs) 
            | x == y = acc
            | otherwise = x : acc

-- (c) split
--split :: [a] -> ([a], [a])
--split [] = ([],[])
--split [x] = ([x],[])
--split (x:y:zs) = (x:xs, y:ys)
--  where (xs, ys) = split zs

--foldr accumulador con (l1,l2,turno_der)
--El turno inicial depende de si la lista es par o no
splitFoldr :: [a] -> ([a], [a])
splitFoldr xs = (lista1, lista2)
    where
        (lista1, lista2, _) = foldr f ([],[],even (length xs)) xs  
        f x (lista1, lista2, turno_Der) 
            |turno_Der = (lista1, x : lista2, not turno_Der)
            |otherwise = (x : lista1, lista2, not turno_Der)

--Otra solucion sin turno
--El turno no desapareció…
--se volvió parte de la estructura del acumulador
--Como foldr empiza por el final, el ultimo elemento que procesa (que es el primero de la lista original)
-- va a quedar a la izquierda
splitFoldr2 :: [a] -> ([a], [a])
splitFoldr2 = foldr f ([], [])
  where
    f x (l1, l2) = (x:l2, l1)

-- (d) takeWhile
--Revierto la lista 2 veces
--  dentro del foldr para que busque en orden
--  y al final para que me de los elementos en orden, sino por como funciona el foldr me los da al reves
--Uso una bandera booleana para saber cuando encuentro el primer elemento que no cumple la condicion
takeWhileFoldr cond xs = reverse lista
    where 
        (lista,_) = foldr f ([],False) (reverse xs) --revierto para buscar en orden
        f x (lista, encontre)  
            | cond x && not encontre = (x : lista, False)
            | otherwise = (lista, True)

--Manera mas simple xD
--Cuando encuentra un elemento que no cumple, rompe la solucion 
--Por eso por mas que comience por el final descarta todos los elementos cuando encuentra uno que no cumple
--Construyendo la lista con los ultimos que evalua que cumplen, que serian los primeros de la lista 
takeWhileFoldr2 cond = foldr f []
    where
        f x acc 
            | cond x = x : acc
            | otherwise = []

----------------Ejercicio 5--------------------
-- Defina las siguientes funciones por recursión estructural usando tail-recursion.
--Una función es tail recursive si no queda ningún cálculo pendiente
--después de la llamada recursiva.

-- (a) sumSqs
sumSqsTR :: Num a => [a] -> a
sumSqsTR xs = f xs 0  
    where 
        f [] acc = acc
        f (x:xs) acc =  f xs acc + x^2

-- (b) elem
elemTR :: Eq a => a -> [a] -> Bool
elemTR e xs = f xs 
    where 
        f [] = False
        f (x:xs)  
            | x == e = True
            | otherwise = f xs 

-- (c) elimDups
elemDupsTR :: Eq a => [a] -> [a]
elemDupsTR (x:xs) = f xs x True
    where 
        f xs anterior True = x : f xs x False
        f [] anterior False = []
        f (x:xs) anterior False
            | x == anterior = f xs anterior False
            | otherwise = x : f xs x False

-- (d) split
splitTR :: [a] -> ([a] , [a])
splitTR xs = f xs True ([], [])
    where 
        f [] turnoIzq (ys , zs) = (ys, zs)
        f (x:xs) turnoIzq (ys, zs) 
            | turnoIzq = f xs False (ys ++ [x], zs)
            | otherwise = f xs True (ys, zs ++ [x])
 
-- (e) maxInd

maxIndTR :: Ord a => [a] -> (a, Int)
maxIndTR (x:xs) = maxIndAcc 1 1 x xs 

maxIndAcc :: Ord a => Int -> Int -> a -> [a] -> (a, Int)
maxIndAcc i acc x [] = (x , acc)
maxIndAcc i acc x (y:xs) 
    | x < y = maxIndAcc (i+1) i y xs
    | otherwise = maxIndAcc (i+1) acc x xs

--Version con where
maxIndTR1 :: Ord a => [a] -> (a, Int)
maxIndTR1 (x:xs) = maxIndAcc1 1 1 x xs 
    where maxIndAcc1 i acc x [] = (x , acc)
          maxIndAcc1 i acc x (y:xs) 
            | x < y = maxIndAcc (i+1) i y xs
            | otherwise = maxIndAcc (i+1) acc x xs

-- (f) takeWhile
takeWhileTR :: (a -> Bool) -> [a] -> [a]
takeWhileTR cond xs = f xs [] 
    where 
        f [] lista = lista 
        f (x:xs) lista 
            | cond x = f xs (lista ++ [x])
            | otherwise = lista

-- (g) dropWhile
dropWhileTR :: (a -> Bool) -> [a] -> [a]
dropWhileTR cond xs = f xs
    where 
        f [] = [] 
        f (x:xs)  
            | cond x = f xs 
            | otherwise = x : xs

----------------Ejercicio 6--------------------
-- Defina la función sumSqs como foldl.
sumSqsFoldl  :: Num a => [a] -> a
sumSqsFoldl = foldl (\acc x  -> x^2 + acc) 0 


----------------Ejercicio 7--------------------
-- Sea h x xs = x - sum xs.
-- ¿Cuál de las siguientes afirmaciones es correcta?

-- (a) h x xs = foldr (-) x xs
-- (b) h x xs = foldl (-) x xs

-- la a es correcta ya que resta de izquierda a derecha 


----------------Ejercicio 8--------------------
-- Una implementación de elem como foldr es más eficiente que con foldl.
-- Definir ambas versiones y comparar con:
-- elem 1 [1..10000000]
-- Explicar por qué una es más eficiente.

elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr e = foldr f False
  where
    f x res =  (x == e) || res --Otra forma de escribir un or

elemFoldl :: Eq a => a -> [a] -> Bool
elemFoldl e = foldl f False
    where 
        f res x = (x == e) || res

--Foldr es mas eficiente dado que no necesita el resultado completo para cortar la ejecucion, mientras que en
--Foldl el acumulador debe tener el resultado completo 

----------------Ejercicio 9--------------------
-- Defina la función maxInd usando foldr y foldl.
maxIndFoldr :: Ord a => [a] -> (a, Int)
maxIndFoldr (x:xs) = foldr f (x, 0) xs
  where
    f y (m, i)
      | y > m     = (y, 0)
      | otherwise = (m, i + 1)     

maxIndFoldl :: Ord a => [a] -> (a, Int)
maxIndFoldl (x:xs) = (m, iMax)
  where
    (m, iMax, _) = foldl f (x, 0, 1) xs -- al final queda algo del estilo (m, iMax, _) = (5,2,4) e ignoro el ultimo 

    f (m, iMax, iAct) y
      | y > m     = (y, iAct, iAct + 1)
      | otherwise = (m, iMax, iAct + 1)   

----------------Ejercicio 10--------------------
-- Defina split y elimDups usando foldl y reverse.
splitFoldl :: [a] -> ([a],[a])
splitFoldl xs = (reverse lista1, reverse lista2)
    where 
        (lista1, lista2,_) = foldl f ([],[],True) xs

        f (l1,l2,turnoIzq) x 
            | turnoIzq = (x : l1, l2, False)
            | otherwise = (l1, x : l2, True)


elimDupsFoldl :: Eq a => [a] -> [a]
elimDupsFoldl (x:xs) = reverse (foldl f [x] xs)
    where 
        f acc@(y:ys) z
            | y /= z = z : acc
            | otherwise = acc

----------------Ejercicio 11--------------------
-- Defina takeWhile usando foldl.
-- Debe usar el acumulador para detectar cuándo terminar.

takeWhileFoldl :: (a -> Bool) -> [a] -> [a]
takeWhileFoldl cond xs = reverse lista
    where
        (lista, _) = foldl f ([], False) xs

        f (lista, encontre) x 
            | cond x && not encontre = (x : lista, encontre)
            | otherwise = (lista, True)


----------------Ejercicio 12--------------------
-- Representación de números como lista de dígitos.

-- (a)
-- sucesor :: [Int] -> [Int]
-- Compute el siguiente número.
sucesorDig :: [Int] -> [Int]
sucesorDig xs = res
    where
        (res, _,_) = foldr f ([],1,length xs) xs

        f x (res, carry, cant) 
            | x + carry > 9 && cant == 1 = ([1, 0] ++ res, 0, 0) -- Primer digito (Unidad)
            | x + carry > 9 = (0 : res, 1, cant - 1) 
            | x + carry <= 9 = ((x + carry) : res, 0, cant - 1)

-- (b)
-- decimal :: [Int] -> Int
-- Convierte la representación a entero.

decimal :: [Int] -> Int
decimal xs = res
    where
        (res,_) = foldr f (0, length xs - 1) xs_reverse
        xs_reverse = reverse xs

        f x (res,cont) = (res + x * 10^cont ,cont-1) 


-- (c)
-- repr :: Int -> [Int]
-- Convierte un entero a su representación.

repr :: Int -> [Int]
repr x = lista
    where 
        (lista,_,_,_) =  f ([],1,0,x) 

        f (lista,pot,anterior_ac,x) 
            | 10^pot <= x = f ((x `mod` 10^pot - anterior_ac) `div` 10^(pot-1) : lista, pot+1,(x `mod` 10^pot - anterior_ac) `div` 10^(pot-1),x)
            | otherwise = ((x `mod` 10^pot - anterior_ac) `div` 10^(pot-1) : lista, pot+1,(x `mod` 10^pot - anterior_ac) `div` 10^(pot-1),x)
        

--Mas simple :D
repr2 :: Int -> [Int]
repr2 0 = [0]
repr2 x
  | x < 10    = [x]
  | otherwise = repr2 (x `div` 10) ++ [x `mod` 10]



----------------Ejercicio 13--------------------
-- Algoritmo de Luhn

-- (a)
-- dobleD :: [Int] -> [Int]
-- Multiplica por 2 cada segundo dígito desde la derecha.

dobleD :: [Int] -> [Int]
dobleD xs = lista
    where 
        (lista, _) = foldr f([],False) xs
        f x (lista, turno) 
            | turno = (x*2 : lista, False)
            | otherwise = (x : lista, True)

-- sumaD :: [Int] -> Int
-- Suma los elementos ajustando los mayores a 9 restando 9.

sumaD :: [Int] -> Int
sumaD = foldr f 0 
    where 
        f x acc 
            | x > 9 = acc + x - 9
            | otherwise = acc + x

--Otra solucion
sumaD2 :: [Int] -> Int
sumaD2 = foldr (\x acc -> acc + if x > 9 then x - 9 else x) 0

-- validar :: Int -> Bool
-- Verifica si es múltiplo de 10.

validar :: Int -> Bool
validar x = x `mod` 10 == 0 

-- (b)
-- luhn :: Int -> Bool
-- Composición de las anteriores.

luhn :: Int -> Bool
luhn = validar. sumaD . dobleD . repr 

-- Como repr x ya es un valor si lo paso como parametro debo separarlos de la siguiente forma
luhn2 :: Int -> Bool
luhn2 x = validar (sumaD (dobleD (repr x)))