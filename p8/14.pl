/*
 Definir el predicado coprimos(-X,-Y), que genere uno a uno todos los pares de números naturales coprimos
 (es decir, cuyo máximo común divisores 1), sin repetir resultados. Usar la función gcd del motor aritmético.
*/
desde2(X,X).
desde2(X, Y) :- nonvar(Y), Y >= X.
desde2(X, Y) :- var(Y), N is X+1, desde2(N, Y).

%coprimos(-X, -Y)
coprimos(X, Y) :- 
        generarPares(X,Y),      % Generate
        gcd(X,Y) =:= 1.         % & Test

%generarPares(-X, -Y)
generarPares(X, Y) :-
        desde2(2, S),            % Uso como generador infinito S = X+Y
        paresSuman(S, X, Y).    

paresSuman(S, X, Y) :-
        S2 is S-1,           % Para evitar que Y sea 0, pues no esta definido el gcd para 0
        between(1, S2, X),   % Instanciamos X entre [1, S-1] 
        Y is S-X.            % Como X ya esta instanciado, calculamos y
    
