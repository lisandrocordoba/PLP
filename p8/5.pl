%  i. last(?L, ?U), donde U es el último elemento de la lista L.

% Version sin append
% last([U], U).
% last([_|YS], U) :- last(YS, U).

last(L, U) :- append(_, [U], L).

% ii. reverse(+L,-L1), donde L1 contiene los mismos elementos que L, pero en orden inverso
reverse([X], [X]).
reverse(L, [X|L2]) :- append(L1, [X], L), reverse(L1, L2).

% iii. prefijo(?P, +L), donde P es prefijo de la lista L.
prefijo(P, L) :- append(P, _, L).

% iv. sufijo(?S, +L), donde S es sufijo de la lista L.
sufijo(S, L) :- append(_, S, L).

% v. sublista(?S, +L), donde S es sublista de L.
% "Los prefijos de los sufijos de una lista L, son todas las sublistas de L"
sublista(SL, L) :- sufijo(S, L), prefijo(SL, S).

% vi. pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L. (Este predicado ya viene
% definido en Prolog y se llama member).

pertenece(X, L) :- append(_, [X|_], L).

% OBS: este predicado es MEMBER en prolog