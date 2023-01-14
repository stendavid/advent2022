%------------------------------
% Matrix (list of lists)
%------------------------------
matrix_size(Matrix, (Width,Height)) :-
    length(Matrix,Height),
    maplist({Width}/[Row]>>length(Row,Width), Matrix).

matrix_size_filled(Matrix, Size, Value) :-
    matrix_size(Matrix, Size),
    maplist(maplist(=(Value)),Matrix).

set_list_value([H0|T0],N,V,[H1|T1]) :-
    (N = 0 ->
        H1 = V, T1 = T0
    ;
        H1 = H0,
        N1 is N-1,
        set_list_value(T0,N1,V,T1)
    ).

set_matrix_value(M0,(X,Y),V,M1) :-
    nth0(Y,M0,Row0),
    set_list_value(Row0,X,V,Row1),
    set_list_value(M0,Y,Row1,M1).

matrix_value(M,(X,Y),Value) :-
    nth0(Y, M, Row), nth0(X, Row, Value).
