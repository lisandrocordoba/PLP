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
