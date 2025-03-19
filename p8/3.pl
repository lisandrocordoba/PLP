natural(0).
natural(suc(X)) :- natural(X).

% Version rota
% menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
% menorOIgual(X,X) :- natural(X).


/*
?- menorOIgual(0,X).
X := suc(Y)
Y := suc(Y')
Y' := suc(Y'')
...
Intenta unificar X con un numero cada vez mas grande X:=  suc(suc(suc(suc(...))))

El problema con esto es que el caso recursivo esta antes que el caso base. Prolog resuelve las reglas de arriba 
abajo y de izquierda a derecha. Es importante que el caso baso este primero y que el caso recursivo corte en algun momento.
*/

% Version corregida

menorOIgual(X, X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y). 