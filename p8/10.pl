/* 
I) 
    Si X no esta instanciada -> Se rompe, pues (N is X+1 produce un error)
    Si Y esta instanciada -> Se cuelga, pues solo unifica con X a lo suma una vez, y luego
    siempre tendremos X > Y, por lo que nunca mas va a instanciarse Y.
    Para que funcione correctamente deberian estar instanciadas tanto X como Y
*/

% II)
% desde(+X, ?Y)
desde2(X,X).
desde2(X, Y) :- nonvar(Y), Y >= X.
desde2(X, Y) :- var(Y), N is X+1, desde2(N, Y).
