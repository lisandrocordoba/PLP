import Prelude hiding (subtract)
---------------------------------------------------
----------------- FALTA EL 7) ---------------------
---------------------------------------------------

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

map :: (a -> b) -> [a] -> [b]
aplica f a cada elemento de la lista

concatMap :: (a -> [b]) -> [a] -> [b]
por cada elemento de la lista, genera otra lista con la funcion
devuelve la concatenacion de todas las listas generadas

take :: Int -> [a] -> [a]
devuelve los primeros i elementos de la lista

drop :: Int -> [a] -> [a]
devuelve la lista a partir del (i+1)esimo elemento
--}
permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x rec -> 
                    concatMap (\r -> 
                        map (\i -> drop i r ++ [x] ++ take i r) [0..length r]
                    ) rec
                ) [[]]

---- II)
{-
partes [1,2,3] = [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]
si ya tengo partes de [2,3] = [[], [2], [3], [2,3]]
que quiero hacer con el 1?
    agregarselo a cada sublista, pero dejando tambien la sublista original
    notar que voy a duplicar el tamaño, pues me quedo con la sublista la original mas la sublista agregandole el 1
    notar que es esperable duplicar el tamaño pues partes(C) = 2 ^ length(C)
-}
partes :: [a] -> [[a]]
partes = foldr (\x rec -> rec ++ map (x:) rec) [[]]

---- III)
{-
prefijos [5,1,2] = [[], [5], [5,1], [5,1,2]]
si ya tengo prefijos [1,2] = [[], [1], [1,2]]
que quiero hacer con el 5?
    agregarselo adelante a cada sublista
-}
prefijos :: [a] -> [[a]]
prefijos = foldr (\x rec -> [] : map (x:) rec) [[]]

---- IV)
{-
sublistas [5,1,2] = [[], [5], [1], [2], [5,1], [1,2], [5,1,2]]
si ya tengo sublistas [1,2] = [[], [1], [2], [1,2]]
que quiero hacer con el 5?
-}
sublistas :: [a] -> [[a]]
sublistas = recr (\x xs r-> map (x:) (prefijos xs) ++ r) [[]]

-- Ejercicio 5
---- I)
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs 
                                        then [x]
                                        else x : elementosEnPosicionesPares (tail xs)

-- No es recursion estructural pues utiliza el valor de XS, y hace recursion sobre tail XS

---- II)
entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                            then x : entrelazar xs []
                            else x : head ys : entrelazar xs (tail ys)

-- Es recursion estructural, pues solo utiliza X y la recursion de XS

entrelazar' :: [a] -> [a] -> [a]
entrelazar' = foldr (\x rec ys -> if null ys then x : rec []
                                    else x : head ys : rec (tail ys)                    
                        ) id
-- Ejercicio 6
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- a)
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if e == x
                                then xs
                                else x : rec) []
                                
-- b) foldr no es adecuado para sacarUna pues con recursion estructural no podria acceder a xs.
-- lo que podria hacer es otra funcion sacarTodas

-- c)
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr(\x xs rec -> if e <= x then  e : x : xs 
                                        else x : rec
                    ) [e]
-- Ejercicio 7


-- Ejercicio 8
---- I)
{- 
mapPares es una versión de map que toma una función currificada de dos argumentos y una lista de pares
de valores, y devuelve la lista de aplicaciones de la función a cada par
 -}
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (Main.uncurry f)

---- II)
