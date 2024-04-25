import Prelude hiding (subtract)
import GHC.Num.BigNat (bigNatAdd)
import Control.Arrow (ArrowZero(zeroArrow))
import Foreign (Bits(zeroBits))
---------------------------------------------------
----------------- FALTA EL 7) ---------------------
----------------- FALTA EL 9) ---------------------
----------------- FALTA EL ) ---------------------

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
armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x rec ys -> 
                        if null ys then []
                        else (x, head ys) : rec (tail ys)
                   ) (const [])

---- III)
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x rec ys -> 
                    if null ys then []
                    else f x (head ys) : rec (tail ys)
             ) (const [])

-- Ejercicio 10
generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs
    | stop xs = init xs
    | otherwise = generateFrom stop next (xs ++ [next xs])

---- I)
generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop base next = generate stop (\xs -> if null xs then base else next (last xs))

---- II)
factoriales :: Int -> [Int]
factoriales n = generate (\xs -> length xs > n) (\xs-> if null xs then 1 else last xs * (length xs + 1))

---- III)
iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase (\xs -> length xs > n) x f

---- IV)
generateFrom' :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom' stop next xs = last (takeWhile (not . stop) (iterate (\ys -> ys ++ [next ys]) xs))

-- Ejercicio 11
---- I)
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

---- II)
potencia :: Integer -> Integer
potencia n = foldNat (\_ rec -> n * rec) 1 n

-- Ejercicio 12
data Polinomio a = X
                | Cte a
                | Suma (Polinomio a) (Polinomio a)
                | Prod (Polinomio a) (Polinomio a)

foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli fX fCte fSuma fProd p = case p of
    X -> fX
    Cte c -> fCte c
    Suma p q -> fSuma (rec p) (rec q)
    Prod p q -> fProd (rec p) (rec q)
    where rec = foldPoli fX fCte fSuma fProd

evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPoli n id (+) (*)

-- Ejercicio 13
data AB a = Nil | Bin (AB a) a (AB a)

---- I)
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB z fBin ab = case ab of
    Nil -> z
    Bin l n r -> fBin (rec l) n (rec r)
    where rec = foldAB z fBin

recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> AB a -> b
recAB z fBin ab = case ab of
    Nil -> z
    Bin l n r -> fBin l n r (rec l) (rec r)
    where rec = recAB z fBin

---- II)
esNil :: AB a -> Bool
esNil ab = case ab of
    Nil -> True
    Bin _ _ _ -> False

altura :: AB a -> Int
altura = foldAB 0 (\recL _ recR -> 1 + max recL recR) 

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\recL _ recR -> 1 + recL + recR)

--- III)
mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB comp (Bin l n r) = foldAB n (\recL n recR -> f recL (f n recR)) (Bin l n r)
    where f x y = if comp x y then x else y

--- IV) NO SE SI ESTA BIEN

-- No hago el caso con el constructor Nil, pues no sabria que devolver
-- Notar que no se rompe pues esABB abarca el caso base Nil devolviendo True
valorNodo :: AB a -> a
valorNodo (Bin l n r) = n 

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\l n r recL recR -> n >= valorNodo l && n < valorNodo r)

-- Ejercicio 14
--- I)
ramas :: AB a -> [[a]]
ramas = foldAB [] f
    where f recL n recR | null recL && null recR = [[n]]
                        | null recR = map (n:) recL
                        | null recL = map (n:) recR
                        | otherwise = map (n:) recL ++ map (n:) recR

cantHojas :: AB a -> Int
cantHojas = foldAB 0 (\recL n recR -> if recL == 0 && recR == 0 then 1 else 0 + recL + recR)

espejo :: AB a -> AB a
espejo = foldAB Nil (\recL n recR -> Bin recR n recL)
