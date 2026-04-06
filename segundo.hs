--Practico 2----------------------------------------------------------
---------------------------Ejercicio 1---------------------------------
--Dada la siguiente funci´on
--dup x = (x , x )
--Explique en que difieren (dup ◦ dup) y (dup dup).

dup :: x -> (x, x)
dup x = (x , x)

--(dup . dup) :: a -> ((a, a), (a, a))
--(dup . dup) x = dup(dup x) = dup (x,x) = ((x,x), (x,x))

--aplica dup sobre dup y no sobre un tipo, entonces error
--(dup dup) x = 

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
    |xs == [] = x
    |otherwise = x + y + sumaLista xs

sumaPrimeros :: [Int] -> [Int]
sumaPrimeros ([]) = []
sumaPrimeros (x : []) = [x] 
sumaPrimeros (x : y : xs) 
    | [x] /= [] && [y] /= [] = [x+y] ++ (x : y : xs)
    | otherwise = [x]



---------------------------Ejercicio 5----------------------------------------

--La funcion flip tiene el siguiente tipo: (a → b → c) → b → a → c.
--Observando el tipo, ¿puede determinar que hace la funcion?
--(a) Implemente la funci´on flip.
--(b) Defina una expresi´on lambda equivalente a la funci´on flip.

--toma una funcion a->b->c que toma un a y b y retorna un c
--y retorna otra funcion (b->a->c) que toma un b y un a y retorna un c
flip :: (a -> b -> c) -> b -> a -> c
flip f x y  = f y x

flip \f x y -> f y x
