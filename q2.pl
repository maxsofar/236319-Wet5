pythagorean(X, Y, Z) :- 
    X > 0, 
    Y > 0, 
    Z > 0, 
    Z * Z =:= X * X + Y * Y.

divide(X, Y) :- 0 is Y mod X.

has_divisor(N, M) :- M * M =< N, (divide(M, N); M2 is M + 2, has_divisor(N, M2)).

prime(2).

prime(N) :- 
    N > 1, 
    N mod 2 =\= 0, 
    \+ has_divisor(N, 3).

goldbach(A, B, C) :- 
    between(2, C, A), 
    B is C - A, 
    B >= 2, 
    prime(A), 
    prime(B).

