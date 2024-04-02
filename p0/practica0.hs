-- Ejercicio 1
{-

null :: [a] -> Bool
define si una lista esta vacia ó no

head :: [a] -> a
devuelve del primer elemento de una lista

tail :: [a] -> [a]
devuelve la lista sin el primer elemento

init :: [a] -> [a]
devuelve la lista sin el último elemento

take :: Int -> [a] -> [a]
devuelve la lista con los primeros N elementos de la lista pasada como parametro

drop :: Int -> [a] -> [a]
devuelve la lista resultante de quitar los primeros N elementos

(++) :: [a] -> [a] -> [a]
devuelve la lista resultante de unir la segunda lista a la primera

concat :: [[a]] -> [a]
devuelve la lista resultante de unir todas las listas

(!!) :: [a] -> Int -> a
devuelve el valor que se encuentra en la N-esima posición

elem :: Int -> [a] -> Bool
devuelve True si el elemento pertenece a la lista
-}

-- Ejercicio 2
--a) valorAbsoluto :: Float → Float, que dado un número devuelve su valor absoluto.

valorAbsoluto :: Float -> Float
valorAbsoluto x | x < 0 = (-1) * x
                | otherwise = x

--b) bisiesto :: Int → Bool, que dado un número que representa un año, indica si el mismo es bisiesto.
bisiesto :: Int -> Bool
bisiesto n | not (esMultiploDe n 4) = False
           | esMultiploDe n 100 && not (esMultiploDe n 400) = False
           | otherwise = True

esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m = mod n m == 0

--c) factorial :: Int → Int, definida únicamente para enteros positivos, que computa el factorial.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

--d) cantDivisoresPrimos :: Int → Int, que dado un entero positivo devuelve la cantidad de divisores primos.

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length (filter esPrimo (divisoresDe n))

esPrimo :: Int -> Bool
esPrimo n = (length (divisoresDe n) == 2)

divisoresDe :: Int -> [Int]
divisoresDe n = auxDivisoresDe n n

auxDivisoresDe :: Int -> Int -> [Int]
auxDivisoresDe _ 0 = []
auxDivisoresDe n i | esMultiploDe n i = i : (auxDivisoresDe n (i - 1)) 
                   | otherwise = (auxDivisoresDe n (i - 1))

-- Ejercicio 3
--Contamos con los tipos Maybe y Either definidos como sigue:
-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

--a)
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just (1 / n)

--b)
aEntero :: Either Int Bool -> Int
aEntero (Right False) = 0
aEntero (Right True) = 1
aEntero (Left n) = n

-- Ejercicio 4
--a) limpiar :: String → String → String, que elimina todas las apariciones de cualquier carácter de la primera
-- cadena en la segunda. Por ejemplo, limpiar ``susto'' ``puerta'' evalúa a ``pera''. Nota: String es un
-- renombre de [Char]. La notación ``hola'' es equivalente a [`h',`o',`l',`a'] y a `h':`o':`l':`a':[].

limpiar :: String -> String -> String
limpiar xs ys = filter (\ y -> not (elem y xs)) ys

--b) difPromedio :: [Float] → [Float] que dada una lista de números devuelve la diferencia de cada uno con el
-- promedio general. Por ejemplo, difPromedio [2, 3, 4] evalúa a [-1, 0, 1].

difPromedio :: [Float] -> [Float]
difPromedio xs = map (\ x -> x - (promedioLista xs)) xs

promedioLista :: [Float] -> Float
promedioLista xs = sumaTotal xs / (fromIntegral (length xs))

sumaTotal :: [Float] -> Float
sumaTotal [] = 0
sumaTotal (x:xs) = x + sumaTotal xs

--c) todosIguales :: [Int] → Bool que indica si una lista de enteros tiene todos sus elementos iguales

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) = elem x xs && todosIguales xs

-- Ejercicio 5 

---------------- TESTEAR --------------------------

-- Dado el siguiente modelo para árboles binarios:
data AB a = Nil | Bin (AB a) a (AB a)

--a) vacioAB :: AB a → Bool que indica si un árbol es vacío (i.e. no tiene nodos)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

--b) negacionAB :: AB Bool → AB Bool que dado un árbol de booleanos construye otro formado por la negación de cada uno de los nodos.
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin l n r) = (Bin (negacionAB l) (not n) (negacionAB r))

--c) productoAB :: AB Int → Int que calcula el producto de todos los nodos del árbol
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin l n r) = n * productoAB l * productoAB r
