import Prelude hiding (subtract)
import Distribution.Simple.Flag (BooleanFlag)
-- Ejercicio 1

max2 :: (Float, Float) -> Float
max2 (x, y) | x >= y = x
            | otherwise = y

-- CURRIFICADA
max2Curry :: Float -> Float -> Float
max2Curry x y | x >= y = x
              |   otherwise = y

----
normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

-- CURRIFICADA 
normaVectorialCurry :: Float -> Float -> Float
normaVectorialCurry x y = sqrt (x^2 + y^2)

----
subtract :: Float -> Float -> Float
subtract = flip (-) 

----
predecesor :: Float -> Float
predecesor = subtract 1

----
evaluarEnCero :: Num a => (a -> b) -> b
evaluarEnCero = \f-> f 0

----
dosVeces :: (a -> a) -> a -> a
dosVeces = \f-> f . f

----
flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

----
flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip



-- Ejercicio 2
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y) 

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- Ejercicio 3
---- I)
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' e = foldr (\x rec -> x == e || rec) False

myAppend :: [a] -> [a] -> [a]
myAppend xs ys = foldr (:) ys xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x rec -> if p x then x:rec else rec) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []


---- II)
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun comp = foldr1 (\x y -> if comp x y then x else y)

---- III)
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldr (\x rec -> x : map (x +) rec) []

---- IV)
sumAlt :: Num a => [a] -> a
sumAlt = foldr (-) 0

---- V)
--sumAltInv :: Num a => [a] -> a
--umAltInv = recr (-) 0

sumAltInv :: Num a => [a] -> a
sumAltInv = foldl (flip (-)) 0

-- Ejercicio 4
---- I)
{--
Pensemos permutaciones [1,2,3]
Si yo ya tengo la recursion de permutaciones [2,3]
Que tengo que hacer con el 1?
    Agregarlo adelante, agregarlo entre los numeros y agregarlo atras
    permutaciones [1,2,3]
        [[1,2,3], [2,1,3], [2,3,1], [1,3,2], [3,1,2], [3,2,1]]

--}
permutaciones :: [a] -> [[a]]
permutaciones = foldr (\head rec -> concatMap (\perm -> (x : xs) ++ ((take 1 xs) : x : (drop 1 xs)) ++ (xs ++ [x])) rec) [] 

{--
concatMap :: (a -> [b]) -> [a] -> [b]
por cada elemento de la lista, genera otra lista con la funcion
devuelve la concatenacion de todas las listas generadas

take :: Int -> [a] -> [a]
devuelve los primeros i elementos de la lista

drop :: Int -> [a] -> [a]
devuelve la lista a partir del (i+1)esimo elemento

--}

