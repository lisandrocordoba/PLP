% Definir el predicado juntar(?Lista1,?Lista2,?Lista3), que tiene éxito si Lista3 es la
% concatenación de Lista1 y Lista2.

juntar([], L2, L2).
juntar([X|L1], L2, [X|L3]) :- juntar(L1, L2, L3). 

% OBS: es el predicado APPEND de prolog