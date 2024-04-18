import Data.Either ()
import Data.List ()

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos 

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id -}

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True) 

{--
universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o) --}

{-Ejercicio 1-}

-- f: constructor Personaje, g: constructor Mueve, h: constructor muere
foldPersonaje :: (Posición -> String -> a ) -> ( a -> Dirección -> a) -> (a -> a) -> Personaje -> a
foldPersonaje f g h (Personaje pos nom) = f pos nom
foldPersonaje f g h (Mueve pers dir) = g (foldPersonaje f g h pers) dir
foldPersonaje f g h (Muere pers) = h (foldPersonaje f g h pers)

-- f: constructor Objeto, g: constructor Tomado, h: constructor EsDestruido
foldObjeto :: (Posición -> String -> a ) -> ( a -> Personaje -> a) -> (a -> a) -> Objeto -> a
foldObjeto f g h (Objeto pos nom) = f pos nom
foldObjeto f g h (Tomado obj pers) = g (foldObjeto f g h obj) pers
foldObjeto f g h (EsDestruido obj) = h (foldObjeto f g h obj)

{-Ejercicio 2-}

--hay que definir una aux que lea el constructor Mueve
posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje (flip (const id)) (aux) id

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (const id) const id

{-Ejercicio 3-}

--objetos_en :: ?
--objetos_en = ?

--personajes_en :: ?
--personajes_en = ?

{-Ejercicio 4-}

--objetos_en_posesión_de :: ?
--objetos_en_posesión_de = ?

{-Ejercicio 5-}

-- Asume que hay al menos un objeto
--objeto_libre_mas_cercano :: ?
--objeto_libre_mas_cercano = ?

{-Ejercicio 6-}

--tiene_thanos_todas_las_gemas :: ?
--tiene_thanos_todas_las_gemas = ?

{-Ejercicio 7-}

--podemos_ganarle_a_thanos :: ?
--podemos_ganarle_a_thanos = ?

o1 = Objeto (0,0) "GemaDorada"
p1 = Personaje (0,0) "Wanda"
o2 = Tomado o1 p1
o3 = EsDestruido o2
p2 = Mueve p1 Este
p3 = Muere p2