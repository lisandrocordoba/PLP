{-- Tipos --}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Data.Either
import Data.List

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

--Observadores y funciones básicas de los tipos 

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)


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
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o) 


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

posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje (flip (const id)) siguiente_posición id

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (const id) const id

{-Ejercicio 3-}
{-
(NOTAR: para mayor comodidad reemplazamos el tipo u del enunciado por el tipo us)
Probar la sgte propiedad: 
∀us::Universo . ∀o::Objeto . elem o (objetos_en us) ⇒ elem (Right o) us
--
Lo probaremos por induccion estructural sobre Universos
Como Universo = [Either Personaje Objeto], tiene dos constructores
Constructor base -> [] 
Constructor recursivo -> (u:us), con u::Either Personaje Objeto y us::[u]

Qpq ∀us::Universo. P(us)
Con P(us) = ∀o::Objeto . elem o (objetos_en us) ⇒ elem (Right o) us

CASO BASE: P([])
  P([]) = ∀o::Objeto . elem o (objetos_en []) ⇒ elem (Right o) []
  = ∀o::Objeto . elem o (map objeto_de (filter es_un_objeto [])) ⇒ elem (Right o) [] ---- Por definición de objetos_de
  = ∀o::Objeto . elem o (map objeto_de []) ⇒ elem (Right o) [] ---- Por definición de filter
  = ∀o::Objeto . elem o [] ⇒ elem (Right o) [] ---- Por definición de map
  = ∀o::Objeto . False ⇒ elem (Right o) [] ---- Por definición de elem
  = True ---- trivialmente

  Asi, vale el caso base P([])

CASO INDUCTIVO: ∀us::[u]. P(us) => ∀u::Either Personaje Objeto. P(u:us)
  HI: P(us) = ∀o::Objeto . elem o (objetos_en us) ⇒ elem (Right o) us
  TI: P(u:us) = ∀o::Objeto . elem o (objetos_en (u:us)) ⇒ elem (Right o) (u:us)

  Qvq vale P(u:us)
  Si el antecedente no vale, la implicación vale trivialmente

  Si el antecedente vale, qvq elem (Right o) (u:us)
    Lo probamos:
    elem (Right o) (u:us)
    = (Right o) == u || elem (Right o) us ---- Por definición de elem

    Separo en 2 casos
    CASO (Right o) == u 
      TRUE || elem (Right o) us
      = TRUE ---- vale trivialmente

    CASO (Right o) != u
      FALSE || elem (Right o) us
      = elem (Right o) us

      Sabemos que
      elem o (objetos_en us) ⇒ elem (Right o) us ---- Por HI

      Y tambien sabemos que
      elem o (objetos_en (u:us)) ---- Asumido en este caso

      Luego, por transitividad, basta probar que
      elem o (objetos_en (u:us)) => elem o (objetos_en us)

      Vemos que el antecedente es equiv a
      elem o (map objeto_de (filter es_un_objeto (u:us))) -- Por definición de objetos_en

      Separamos en 2 casos
        Si (es_un_objeto u)
          Podemos interpretar u como Right u'
          Como sabemos que Right o != Right u' ---- Pues estamos en el caso (Right o) != u
          Vale o != u'

        Si not(es_un_objeto u)
          Sabemos que u no pertenecerá a objetos_en --- Por definición objetos_en

      En ambos casos, concluimos que el antecedente elem o (objetos_en (u:us))
      se puede expresar como elem o (objetos_en us)

      Luego, 
      qvq elem o (objetos_en us) => elem o (objetos_en us)
      Vale trivialmente

  Así, ∀us::[u]. P(us) => ∀u::Either Personaje Objeto. P(u:us)
-}

objetos_en :: Universo -> [Objeto]
objetos_en u = map objeto_de (filter es_un_objeto u)

personajes_en :: Universo -> [Personaje]
personajes_en u = map personaje_de (filter es_un_personaje u)

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> Universo -> [Objeto] ------ !!!! Chequear q si el personaje esta muerto debe retornar []
objetos_en_posesión_de s u = filter (en_posesión_de s) (objetos_en u)


{-Ejercicio 5-}

-- Asume que hay al menos un objeto
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto ---- !!!! Chequear los q quedaron libres porq murio su personaje
objeto_libre_mas_cercano p u = foldr1 (\x y -> if distancia (Left p) (Right x) < distancia (Left p) (Right y) then x else y) (objetos_libres_en u)

{-Ejercicio 6-}

tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = length (filter es_una_gema (objetos_en_posesión_de "Thanos" u)) >= 6 && está_el_personaje "Thanos" u && está_vivo (personaje_de_nombre "Thanos" u)

{-Ejercicio 7-}

podemos_ganarle_a_thanos :: Universo -> Bool -- VOLVER A VER Q CONDICIONES NECESITO
podemos_ganarle_a_thanos u = not (tiene_thanos_todas_las_gemas u) && ((está_el_personaje "Thor" u && está_vivo (personaje_de_nombre "Thor" u) && está_el_objeto "StormBreaker" u && not (fue_destruido (objeto_de_nombre "StormBreaker" u))) || (está_el_personaje "Wanda" u && está_vivo (personaje_de_nombre "Wanda" u) && está_el_personaje "Visión" u && está_vivo (personaje_de_nombre "Visión" u) && en_posesión_de "Visión" (objeto_de_nombre "Gema de la Mente" u) && not (fue_destruido (objeto_de_nombre "Gema de la Mente" u))))


t1 = Personaje (0,0) "Thanos"
--t2 = Personaje (0,0) "Thanos"  -- PREGUNTAR QUE PASA SI HAY 2 "Thanos".
p1 = Personaje (0,0) "Visión"
p2 = Personaje (0,0) "Thor"
p3 = Personaje (0,0) "Wanda"
o1 = Objeto (0,0) "Gema de 1"
o2 = Objeto (0,0) "Gema de 2"
o3 = Objeto (0,0) "Gema de 3"
o4 = Objeto (0,0) "Gema de 4"
o5 = Objeto (0,0) "Gema de 5"
oM = Objeto (0,0) "Gema de la Mente"

o7 = Objeto (0,0) "StormBreaker"

thormuerto = Muere p2

tom1 = Tomado o1 t1
tom2 = Tomado o2 t1
tom3 = Tomado o3 t1
tom4 = Tomado o4 t1
tom5 = Tomado o5 t1
tom6 = Tomado oM t1

tomVision = Tomado oM p1

universoT = [Left t1, Right tom1, Right tom2, Right tom3, Right tom4, Right tom5, Right tom6]
universoTPeroconThor = [Left t1, Right tom1, Right tom2, Right tom3, Right tom4, Right tom5, Right tom6, Left p2, Right o7]

universosinT = [Right tom1, Right tom2, Right tom3, Right tom4, Right tom5, Right tom6, Left p2, Right o7]


universoTPeroWanda = [Left t1, Right tom1, Right tom2, Right tom3, Right tom4, Right tom5, Right tom6, Left p1, Right tomVision, Left p3]
universoTPeroWanda2 = [Left t1, Right tom1, Right tom2, Right tom3, Right tom4, Right tom5, Left p1, Right oM, Left p3]
universoTPeroWanda3 = [Left t1, Right tom1, Right tom2, Right tom3, Right tom4, Right tom5, Left p1, Right tomVision]
universoTPeroWanda4 = [Left t1, Right tom1, Right tom2, Right tom3, Right tom4, Right tom5, Left p3, Right tomVision]
universoTPeroWanda5V = [Left t1, Right tom1, Right tom2, Right tom3, Right tom4, Right tom5, Left p1, Right tomVision, Left p3]


universoTfalso = [Left t1, Right tom1, Right tom2, Right tom3, Right tom4]

universoThorGana = [Left p2, Right o7]
universoThorGanariaPeroMurio = [Left thormuerto, Right o7]
universoThorWanda = [Left p2, Right o7, Left p1, Right tomVision, Left p3]

obj1 = Objeto (0,0) "Gema del Tiempo"
obj2 = Objeto (0,0) "Gema del viento"

obj3 = Objeto (0,0) "Gema de 33"

m1 = Tomado obj1 p1
m2 = Tomado obj2 p1
pp1 = foldPersonaje Personaje Mueve Muere p1

--o6 = EsDestruido obj3

universo1 :: [Either Personaje Objeto]
universo1 = [Left pp1, Right m2, Left p2, Right m1, Left p3, Right oM]



o = Objeto (0,0) "Cuaderno"
prueba = elem o (objetos_en universo1)