% i)
%cuadradoSemiLatino(+N, -XS)
cuadradoSemiLatino(N, C) :-
            length(C, N),            % Instancio a C como lista de N variables libres
            desde2(0, S),            % Generacion infinita de todas las posibles sumas
            filasQueSumanS(N, S, C). % Pido que cada fila de C sume S

% filasQueSuman(+N, +S, ?C): Instancia C con N filas de largo N que suman S
filasQueSumanS(_, _, []).
filasQueSumanS(N, S, [F|FS]) :- 
            length(F, N),               % Instancia la fila F con N variables libres       
            filaQueSumaS(S, F),
            filasQueSumanS(N, S, FS).

% filaQueSuma(+S, -F): Instancia una fila en N elementos que suman K
filaQueSumaS(0, []).
filaQueSumaS(S, [X|XS]) :- 
            between(0, S, X),       % Instancia X con un numero [0,S]
            S1 is S-X,
            filaQueSumaS(S1, XS).   % Pide que los XS numeros restantes sumen S-X


desde2(X,X).
desde2(X, Y) :- nonvar(Y), Y >= X.
desde2(X, Y) :- var(Y), N is X+1, desde2(N, Y).





% ii) 
%cuadradoMagico(+N, -XS)
cuadradoMagico(N, C) :-
        cuadradoSemiLatino(N, C),        % GENERATE
        nth1(1, C, F1),
        sum_list(F1, MN),                % Instancia el Magic number como la suma de la primer fila
        not((between(1, N, I), columna(I, C, Col), sum_list(Col, SumaCol), SumaCol \== MN)).   % TEST, todas las columnas deben sumar el Magic Number

%columna(+I, +FS, -CS): instancia CS en la columna I-esima de la matriz FS
columna(_, [], []).
columna(I, [F|FS], [C|CS]) :-
        nth1(I, F, C), columna(I, FS, CS).

            
