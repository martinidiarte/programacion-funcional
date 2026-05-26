import Control.Monad.Trans.Cont (reset)

----------------Ejercicio 1--------------------

-- Considere la siguiente definición de los números naturales:

data Nat = Zero | Succ Nat
    deriving Show

-- Ejemplos:
-- Zero
-- Succ Zero 
-- Succ (Succ Zero)
-- De esta forma, la representacion del n-esimo natural es de la forma Succ^n Zero
----------------Ejercicio 1.a--------------------

-- nat2int :: Nat -> Int
-- Convierte un natural en entero

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n 

-- duplica :: Nat -> Nat
-- Retorna el doble del natural

duplica :: Nat -> Nat
duplica Zero = Zero
duplica (Succ n) = Succ (Succ (duplica n))

-- exp2 :: Nat -> Nat
-- Calcula 2^n

-- n=0 exp2 Zero = 1 = Succ Zero
-- n=1 exp2 Succ Zero = 2 = Succ(Succ Zero) 
-- n=2 exp2 Succ(Succ Zero) = 4 = Succ(Succ(Succ(Succ Zero)) 
-- 2^(n+1) = 2 * 2^n


exp2 :: Nat -> Nat
exp2 Zero = Succ Zero
exp2 (Succ n) = duplica (exp2 n)


-- suma :: Nat -> Nat -> Nat
-- Suma de naturales

suma :: Nat -> Nat -> Nat
suma a Zero = a
suma Zero b = b
suma (Succ n1) (Succ n2) = Succ(Succ (suma n1 n2))

--Mas optimo
suma2 :: Nat -> Nat -> Nat
suma2 Zero b = b
suma2 (Succ a) b = Succ (suma2 a b)

-- predecesor :: Nat -> Nat
-- El predecesor de Zero es Zero

predecesor :: Nat -> Nat
predecesor Zero = Zero
predecesor (Succ n) = n


----------------Ejercicio 1.b--------------------

-- Fold para naturales

foldN :: (a -> a) -> a -> Nat -> a
foldN h e Zero = e
foldN h e (Succ n) = h (foldN h e n)

-- Zero   → e
-- Succ   → h
-- h transforma el resultado parcial
-- No el NAT original

-- Reimplementar las funciones anteriores usando foldN

nat2intFold :: Nat -> Int
nat2intFold = foldN h 0 
    where 
        h n = n + 1  

-- Otra forma mas compacta 
nat2intFold2 = foldN (+1) 0

duplicaFold :: Nat -> Nat
duplicaFold = foldN h Zero
    where  
        h n = Succ (Succ n)

-- 2^0 = 1
-- 2^(n+1) = 2 * 2^n = duplicar (2^n)
exp2Fold :: Nat -> Nat 
exp2Fold = foldN h (Succ Zero)
    where 
        h = duplica 

-- sumar = aplicar Succ repetidamente
-- n1 + n2 -> uso n1 como acumulador/base y la estructura recursiva n2
sumaFold :: Nat -> Nat -> Nat
sumaFold = foldN h 
    where 
        h = Succ 

-- Quiero una evolucion asi por eso el estado inicial es (Zero, Zero)
-- 0 → (0,0)
-- 1 → (0,1)
-- 2 → (1,2)
-- 3 → (2,3)
predecesorFold :: Nat -> Nat
predecesorFold n = anterior
    where 
        (anterior, _) = foldN h (Zero, Zero) n
        
        h (anterior, actual) =  (actual,Succ actual) 


----------------Ejercicio 1.c--------------------

-- fib :: Nat -> Nat
-- Implementar Fibonacci:
-- 1) versión recursiva clásica
-- 2) versión lineal/iterativa

fib :: Nat -> Nat
fib Zero = Zero
fib (Succ Zero) = Succ Zero
fib (Succ n) = suma (fib n) (fib (predecesor n))


fibLineal :: Nat -> Nat
fibLineal Zero = Zero
fibLineal (Succ Zero) = Succ Zero
fibLineal n = actual
    where  
        (anterior, actual,_) = f (Succ Zero,Succ Zero,n) 

        f (anterior, actual,Succ Zero)  =  (anterior, actual,Succ Zero)

        f (anterior, actual, n)  = f (actual,suma anterior actual,predecesor n)  

--(anterior, actual) = (fib(n-1), fib(n))

----------------Ejercicio 2--------------------

-- Representación de enteros

data OurInt = IntZero | Pos Nat | Neg Nat
    deriving Show

-- Ejemplos:
-- Pos (Succ Zero)   == 1
-- Neg Zero          == -1


----------------Ejercicio 2.a--------------------

-- Definir instancia Num para OurInt

instance Num OurInt where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined


----------------Ejercicio 2.b--------------------

-- ¿Qué problema tienen estas representaciones?

data OtroInt = OZero | OPos OtroInt | ONeg OtroInt

data OtroInt2 = OPos2 Nat | ONeg2 Nat

-- Escribir comentarios/respuestas acá:



----------------Ejercicio 3--------------------

-- Árbol binario

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving Show


----------------Ejercicio 3.a--------------------

-- inorder :: Tree a -> [a]
-- preorder :: Tree a -> [a]
-- postorder :: Tree a -> [a]

inorder :: Tree a -> [a]
inorder = undefined


preorder :: Tree a -> [a]
preorder = undefined


postorder :: Tree a -> [a]
postorder = undefined


----------------Ejercicio 3.b--------------------

-- mkTree :: Ord a => [a] -> Tree a
-- Construye un árbol binario de búsqueda

mkTree :: Ord a => [a] -> Tree a
mkTree = undefined


-- Posible función auxiliar:
-- insertar :: Ord a => a -> Tree a -> Tree a

insertar :: Ord a => a -> Tree a -> Tree a
insertar = undefined


----------------Ejercicio 3.c--------------------

-- ¿Qué hace inorder . mkTree ?
-- Escribir explicación acá:



----------------Ejercicio 4--------------------

-- Árbol binario con información solo en hojas

data BTree a = Leaf a | Fork (BTree a) (BTree a)
    deriving Show


----------------Ejercicio 4.a--------------------

-- depths :: BTree a -> BTree Int
-- Reemplaza cada hoja por su profundidad

depths :: BTree a -> BTree Int
depths = undefined


----------------Ejercicio 4.b--------------------

-- balanced :: BTree a -> Bool
-- Determina si el árbol está balanceado

-- Sugerencia:
-- primero definir size

sizeBTree :: BTree a -> Int
sizeBTree = undefined


balanced :: BTree a -> Bool
balanced = undefined


----------------Ejercicio 4.c--------------------

-- mkBTree :: [a] -> BTree a
-- Construye un árbol balanceado a partir de una lista no vacía

mkBTree :: [a] -> BTree a
mkBTree = undefined


-- Posible auxiliar:
-- splitMitad :: [a] -> ([a],[a])

splitMitad :: [a] -> ([a],[a])
splitMitad = undefined


----------------Ejercicio 4.d--------------------

-- retrieve :: BTree a -> Int -> a
-- Retorna el valor de la n-ésima hoja

retrieve :: BTree a -> Int -> a
retrieve = undefined



----------------Ejercicio 5--------------------

-- Árbol homogéneo

data HTree a = Tip a | Bin (HTree a) a (HTree a)
    deriving Show


----------------Ejercicio 5.a--------------------

-- mapHT :: (a -> b) -> HTree a -> HTree b

mapHT :: (a -> b) -> HTree a -> HTree b
mapHT = undefined


----------------Ejercicio 5.b--------------------

-- subtrees :: BTree a -> HTree (BTree a)

subtrees :: BTree a -> HTree (BTree a)
subtrees = undefined


----------------Ejercicio 5.c--------------------

-- sizes :: BTree a -> HTree Int

sizes :: BTree a -> HTree Int
sizes = undefined


----------------Ejercicio 5.d--------------------

-- Rehacer sizes usando:
-- 1) mapHT
-- 2) subtrees

sizes2 :: BTree a -> HTree Int
sizes2 = undefined



----------------Ejercicio 6--------------------

-- Clase Sizeable

----------------Ejercicio 6.a--------------------

class Sizeable a where
    size :: a -> Int


-- Instancia para Int

instance Sizeable Int where
    size = undefined


-- Instancia para Char

instance Sizeable Char where
    size = undefined


----------------Ejercicio 6.b--------------------

-- Instancia para listas

instance Sizeable a => Sizeable [a] where
    size = undefined


-- Instancia para pares

instance (Sizeable a, Sizeable b) => Sizeable (a,b) where
    size = undefined


----------------Ejercicio 6.c--------------------

-- Instancia para Tree

instance Sizeable a => Sizeable (Tree a) where
    size = undefined


----------------Ejercicio 6.d--------------------

-- filtLt
-- Retorna elementos con tamaño menor que n

filtLt :: Sizeable a => Int -> [a] -> [a]
filtLt = undefined


----------------Ejercicio 6.e--------------------

-- isSmaller
-- Compara tamaños de tipos Sizeable

isSmaller :: (Sizeable a, Sizeable b) => a -> b -> Bool
isSmaller = undefined


----------------Ejercicio 6.f--------------------

-- Clase Enumerate

class Sizeable a => Enumerate a where
    enum :: Int -> [a]


-- Instancia para Int

instance Enumerate Int where
    enum = undefined


-- Instancia para Char

instance Enumerate Char where
    enum = undefined


-- Instancia para pares

instance (Enumerate a, Enumerate b) => Enumerate (a,b) where
    enum = undefined