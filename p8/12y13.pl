% 12)

/*
 Un árbol binario se representará en Prolog con:
 nil, si es vacío.
 bin(izq, v, der), donde v es el valor del nodo, izq es el subárbol izquierdo y der es el subárbol derecho.
*/

ab1(AB) :- AB = bin(bin(bin(bin(nil, 4, nil), 3, nil), 2, nil), 1, bin(nil, 2, bin(nil, 3, nil))).
l1(L) :- L = [4, 3, 2, 1, 2, 3].

vacio(AB) :- AB = nil.

raiz(bin(_, V, _), V).

altura(nil, 0). 
altura(bin(I, _, D), N) :- altura(I, NI), altura(D, ND), N is 1+max(NI, ND).

cantNodos(nil, 0). 
cantNodos(bin(I, _, D), N) :- cantNodos(I, NI), cantNodos(D, ND), N is 1+NI+ND.

% 13)

inorder(nil, []).
inorder(bin(I, V, D), L) :- inorder(I, LI), inorder(D, LD), append(LI, [V|LD], L).

arbolConInorder([], nil).
arbolConInorder(L, bin(I, V, D)) :- 
            append(LI, [V|LD], L),      % LI y LD son las sublistas de la rlos subarboles I y D
            arbolConInorder(LI, I),
            arbolConInorder(LD, D).

aBB([]).        
aBB(bin(I, V, D)) :- 
            aBB(I), aBB(D),                      % Los subarboles son ABB
            inorder(I, LI), max_list([V|LI], V), % V es mayor a todos los nodos del subarbol izq
            inorder(D, LD), min_list([V|LD], V). % V es menor a todos los nodos del subarbol der

aBBInsertar(X, nil, bin(nil, X, nil)).
aBBInsertar(X, bin(I, X, D), bin(I, X, D)).    % ABB es sin repetidos. Si ya pertenece al arbol no hacemos nada.
aBBInsertar(X, bin(I, V, D), bin(I2, V, D)) :- 
            X < V, aBBInsertar(X, I, I2).      % Insertar en subarbol izq.
aBBInsertar(X, bin(I, V, D), bin(I, V, D2)) :- 
            X > V, aBBInsertar(X, D, D2).      % Insertar en subarbol der.