%  i. intersección(+L1, +L2,-L3), tal que L3 es la intersección sin repeticiones de las listas L1 y L2, respetando en L3 el orden en que aparecen los elementos en L1.
interseccion([], _, []).

% Caso X pertenece a L2 (Para evitar duplicados, una vez que ya agregue un X, elimino todas sus apariciones en L2)
interseccion([X|L1], L2, [X|L4]) :- member(X, L2), borrar(L2, X, L3), interseccion(L1, L3, L4).
% Caso X no pertenece a L2
interseccion([X|L1], L2, L3) :- not(member(X, L2)), interseccion(L1, L2, L3).

% partir(N, L, L1, L2), donde L1 tiene los N primeros elementos de L, y L2 el resto. Si L tiene menos de N
% elementos el predicado debe fallar. ¿Cuán reversible es este predicado? Es decir, ¿qué parámetros pueden
% estar indefinidos al momento de la invocación

/* VERSION VIEJA
Funciona, pero no es reversible, pues si N no esta instanciado, (N>0) y (N1 is N-1) se rompe

partir(0, L, [], L).
partir(N, [X|L], [X|L1], L2) :- N1 is N-1, partir(N1, L, L1, L2).
*/

% Version reversible
partir(0, L, [], L).
partir(N, [X|L], [X|L1], L2) :- partir(N1, L, L1, L2), N is N1+1.


% ii. borrar(+ListaOriginal, +X,-ListaSinXs), que elimina todas las ocurrencias de X de la lista ListaOriginal.
borrar([], _, []).
% Caso X == E
borrar([E|XS], E, YS) :- borrar(XS, E, YS).
% Caso X != E
borrar([X|XS], E, [X|YS]) :- X \= E, borrar(XS, E, YS).

% iii. sacarDuplicados(+L1,-L2), que saca todos los elementos duplicados de la lista L1.
sacarDuplicados([], []).
sacarDuplicados([X|XS], [X|L2]) :- borrar(XS, X, L3), sacarDuplicados(L3, L2). 

% iv. permutación(+L1, ?L2), que tiene éxito cuando L2 es permutación de L1. ¿Hay una manera más eficiente
% de definir este predicado para cuando L2 está instanciada?
% supo tengo permutaciones de XS, que hago con X? lo agrego en cada posicion
permutacion([], []).
permutacion([X|XS], YS) :- permutacion(XS, ZS), insertar(X, ZS, YS).

insertar(X, L, LX) :- append(P, S, L), append(P, [X|S], LX).

% v. reparto(+L, +N,-LListas) que tenga éxito si LListas es una lista de N listas (N ≥ 1) de cualquier
% longitud- incluso vacías- tales que al concatenarlas se obtiene la lista L.
reparto([], 0, []).
reparto(L, N, [X|XS]) :- 
                N > 0,                % Si quedan sublistas que generar
                append(X, L2, L),     % Genero todas las posibles sublistas X (Son prefijos de prefijos de L) 
                N2 is N-1,            % Tengo que repartir L2 en N-1 sublistas
                reparto(L2, N2, XS).  % Genero el resto de las sublistas recursivamente


% vi. repartoSinVacías(+L,-LListas) similar al anterior, pero ninguna de las listas de LListas puede ser
% vacía, y la longitud de LListas puede variar
repartoSinVacias(L, XS) :-
                    length(L, N),
                    between(1, N, K),
                    reparto(L, K, XS),
                    not((member(X, XS), length(X, 0))).

%% [1,2,3,4,5]   [[1,2], [], [3], [4,5], [], []]