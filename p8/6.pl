% Definir el predicado aplanar(+Xs,-Ys), que es verdadero sii Ys contiene los elementos de todos los niveles de
% Xs, en el mismo orden de aparición. Los elementos de Xs son enteros, átomos o nuevamente listas, de modo que
% Xs puede tener una profundidad arbitraria. Por el contrario, Ys es una lista de un solo nivel de profundidad.

aplanar([], []).

% Caso X es una lista
aplanar([X|XS], YS) :- 
            aplanar(X, A1),         % A1 es X aplanado
            aplanar(XS, A2),        % A2 XS aplanada (recursivamente)
            append(A1, A2, YS).     % YS los elementos aplanados en orden

% Caso X no es una lista
aplanar([X|XS], YS) :-
            not(aplanar(X,_)),        % X no es aplanable (no es lista)
            aplanar(XS, A2),        % A2 XS aplanada (recursivamente)
            append([X], A2, YS).      % Y2 es X seguido de los aplanados de XS en orden

% Obs: este predicado es FLATTEN en prolog