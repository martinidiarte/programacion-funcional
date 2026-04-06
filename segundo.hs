--Practico 2----------------------------------------------------------
---------------------------Ejercicio 1---------------------------------
--Dada la siguiente funci´on
--dup x = (x , x )
--Explique en que difieren (dup ◦ dup) y (dup dup).
{- HLINT ignore "Use null" -}

dup :: x -> (x, x)
dup x = (x , x)

--(dup . dup) :: a -> ((a, a), (a, a))
--(dup . dup) x = dup(dup x) = dup (x,x) = ((x,x), (x,x))

--aplica dup sobre dup y no sobre un tipo, entonces error
--(dup dup) x = ?

---------------------------Ejercicio 2---------------------------------
--Dada la funcion twice
--twice f = f ◦ f
--Explique el resultado de hacer twice tail [1, 2, 3, 4]. ¿Es posible hacer
--twice head [1, 2, 3, 4]? Justifique.

--twice tail [1, 2, 3, 4] = tail (tail [1, 2, 3, 4]) = tail [2, 3, 4] = [3, 4]
--twice head [1, 2, 3, 4] = head (head [1, 2, 3, 5]) = head 1 = ERROR ya que head espera una lista


---------------------------Ejercicio 3----------------------------------------
--Sea h x y = f (g x y). ¿Cu´ales de las siguientes afirmaciones son correctas?
--(a) h ≡ f ◦ g         ESTA  SI
--(b) h x ≡ f ◦ g x     ESTA SI
--(c) h x y ≡ (f ◦ g) x y   ESTA NO

---------------------------Ejercicio 4----------------------------------------
--Implemente usando pattern matching una funci´on sumaPrimeros, que
--dada una lista de enteros agrega al principio el resultado de sumar sus
--dos primeros elementos (si tiene). Por ejemplo sumaPrimeros [1, 2, 3, 4]
--resulta en [3, 1, 2, 3, 4], mientras que sumaPrimeros [1] resulta en [1].

--No la pidieron pero pinto
sumaLista :: [Int] -> Int
sumaLista (x : y : xs)
    |null xs = x --xs == []
    |otherwise = x + y + sumaLista xs

sumaPrimeros :: [Int] -> [Int]
sumaPrimeros [] = []
sumaPrimeros [x] = [x]
sumaPrimeros (x : y : xs) = (x+y) : (x : y : xs)


---------------------------Ejercicio 5----------------------------------------

--La funcion flip tiene el siguiente tipo: (a → b → c) → b → a → c.
--Observando el tipo, ¿puede determinar que hace la funcion?
--(a) Implemente la funci´on flip.
--(b) Defina una expresi´on lambda equivalente a la funci´on flip.

--toma una funcion a->b->c que toma un a y b y retorna un c
--y retorna otra funcion (b->a->c) que toma un b y un a y retorna un c
flip :: (a -> b -> c) -> b -> a -> c
flip f x y  = f y x

--la lambda recibe 3 parámetros:
--f   x   y
--pero:
--f debe ser una función
--x e y son los valores que se le pasan a esa función
--O sea, conceptualmente:
--f = una función de dos argumentos
--x = primer valor
--y = segundo valor
--y después hace:
--f y x
--intercambiando x e y

flipLambda = \f x y -> f y x

--ejemplo de ejecucion
--flipLambda (-) 2 3

---------------------------Ejercicio 6----------------------------------------

--Usando secciones y composici´on de funciones, implemente una funci´on
--cuentas ::Integer → Integer , que dado un n´umero, le sume 3, al resultado
--lo multiplique por 2, luego le reste 8 y finalmente lo divida por dos.
--Ejemplo
-- cuentas 3 = 2

cuentas :: Integer -> Integer
--cuentas x = ((`div` 2).(+(-8)).(2*).(3+)) x --Version Humilde
--en Haskell las funciones están “curriadas”: una composición de funciones ya produce otra función
cuentas = (`div` 2).subtract 8.(2*).(3+)

---------------------------Ejercicio 7----------------------------------------

--(a) Implemente la funcion map usando listas por comprension.
--(b) Implemente la funcion filter usando listas por comprension.
--map :: (a → b) → [a] → [b]

mapAux :: (a -> b) -> [a] -> [b]
mapAux _ [] = []
mapAux f (x : xs)  = f x : mapAux f xs

--filter :: (a → Bool) → [a] → [a]

filterAux :: (a -> Bool) -> [a] -> [a]
filterAux _ [] = []
filterAux f (x : xs)
    | f x = x : filterAux f xs
    | otherwise = filterAux f xs

---------------------------Ejercicio 8----------------------------------------

--Usando map, defina una funcion squares :: [Int ] → [Int ] que dada una
--lista de enteros retorne una lista con los cuadrados de los elementos de la
--lista.

squares :: [Int] -> [Int]
squares = map (^2) -- es lo mismo que squares x = map (^2) x

---------------------------Ejercicio 9----------------------------------------

--Defina la funcion length en terminos de map y sum.

lengthAux :: [a] -> Int
lengthAux = sum . map (\_ -> 1)

---------------------------Ejercicio 10----------------------------------------

--Usando filter , defina:
--(a) Una funcion all :: (a → Bool) → [a] → Bool que dada una condicion
--y una lista, verifique si todos los elementos de la lista cumplen con
--dicha condicion. Ejemplos:
--all (>0) [1, 2, 3] retorna True
--all (≡ ’a’) [’a’, ’b’, ’c’] retorna False

--(b) Una funcion elem :: Eq a ⇒ a → [a ] → Bool que determina si un
--elemento pertenece a una lista. Ejemplos:
--elem 2 [1, 2, 3] retorna True
--elem ’a’ [’b’, ’c’] retorna False

allAux :: (a -> Bool) -> [a] -> Bool
allAux _ [] = False
allAux cond xs = length xs == length (filter cond xs) -- Otra forma: allAux cond xs = length xs == (length .filter cond) xs

elemAux :: Eq a => a -> [a] -> Bool
elemAux a xs = length (filter (== a) xs) > 0 -- Otra forma: elemAux a xs = not (null (filter (== a) xs))


---------------------------Ejercicio 11----------------------------------------

--Indique el tipo y explique lo que hace la siguiente funcion:
--rara p = filter p ◦ filter (not ◦ p)

rara :: (a -> Bool) -> [a] -> [a]
rara p = filter p . filter (not . p)

--recibe como parametros una funcion booleana y una lista ya que filter necesita una condicion y una lista
--Y retorna una lista ya que filter retorna una lista 
--Lo que hace esta funcion es dada una lista primero filtra los que no cumplen la condicion p y luego a esa lista
--restante filtra los que cumplen p, entonces retorna la lista vacia 

---------------------------Ejercicio 12----------------------------------------

--12. Indique el tipo y explique lo que hace la siguiente funcion:
--rara2 = zipWith (◦) [length, sum ] [drop 4, take 4]
--Muestre un ejemplo de aplicaci´on correcta de la expresi´on (head rara2 ) y
--su resultado.

-- drop n xs -> elimina los primeros n elementos de la lista xs
-- take n xs -> retorna una lista con los primeros n elementos de xs
-- zipWith op xs ys -> retorna la lista resultado de aplicar la operacion op a xs e ys elemento a elemento

rara2 :: [[Int] -> Int]
rara2 = zipWith (.) [length, sum] [drop 4, take 4]

-- zipWith toma como funcion la composicion (.) y como listas a l1 = [length, sum] y l2 = [drop 4, take 4]
-- luego obtiene [length . drop 4, sum . take 4] 
-- Esto es una lista de funciones [a -> b]
-- En la cabeza la funcion length.drop 4 es decir el tamaño de la lista al sacar los 4 primeros elementos
-- Y el segundo y ultimo elemento sum.take 4 es decir la suma de los primeros 4 elementos de la lista 
-- Como length da un numero entero la salida tiene que ser int, por lo que a y b son Int
-- Por lo que el tipo es [[Int] -> Int]

--head rara2 [1,2,3,4,5] retorna 1 ya que es la cant de elementos de la lista sacandole los 4 primeros
