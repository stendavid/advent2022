:- use_module(library(yall)).
;- [read_map].
:- [matrix].

inf_value(10000).

point(Map,(X,Y),H) :- nth0(Y, Map, Row), nth0(X, Row, H).

height(Map,P,H) :- point(Map,P,H).

map_size(Map, (W,H)) :- length(Map, H), nth0(0, Map, Row), length(Row, W).

can_reach(H0, H1) :- H1 =< H0+1, !; is_end_height(H1), H0 is H1-2. % can reach E from both 'y' and 'z'

can_reachp(Map, P0, P1) :- height(Map, P0, H0), height(Map, P1, H1), can_reach(H0, H1).

move_r(W, (X0,Y0), (X1,Y0)) :- X0 < W-1, X1 is X0+1.
move_d(H, (X0,Y0), (X0,Y1)) :- Y0 < H-1, Y1 is Y0+1.
move_u((X0,Y0), (X0,Y1)) :- Y0 > 0, Y1 is Y0-1.
move_l((X0,Y0), (X1,Y0)) :- X0 > 0, X1 is X0-1.

neighbours(Map, P, Ns) :-
    map_size(Map, (W,H)),
    (move_r(W,P,Nr), can_reachp(Map,P,Nr) -> N0 = [Nr] ; N0 = []),
    (move_d(H,P,Nd), can_reachp(Map,P,Nd) -> N1 = [Nd|N0] ; N1 = N0),
    (move_u(P,Nu),   can_reachp(Map,P,Nu) -> N2 = [Nu|N1] ; N2 = N1),
    (move_l(P,Nl),   can_reachp(Map,P,Nl) -> Ns = [Nl|N2] ; Ns = N2).

update_if_nearer([],_,Distances, Distances, Q, Q).
update_if_nearer([N|Ns],DNext,DistancesIn, DistancesOut, QIn, QOut) :-
    matrix_value(DistancesIn,N,D),
    (DNext < D ->
        set_matrix_value(DistancesIn, N, DNext, Distances0),
        Q0 = [(N,DNext)|QIn]
    ;
        Distances0 = DistancesIn,
        Q0 = QIn
    ),
    update_if_nearer(Ns, DNext, Distances0, DistancesOut, Q0, QOut).

update_nbr_dists(Map, P, QIn, QOut, DistancesIn, DistancesOut) :-
    neighbours(Map, P, Nbrs),
    matrix_value(DistancesIn, P, D),
    DNext is D + 1,
    update_if_nearer(Nbrs,DNext,DistancesIn, DistancesOut, QIn, QOut).

update_dists(_, [], Distances, Distances).
update_dists(Map, [(P,D)|Q], DistancesIn, DistancesOut) :-
    matrix_value(DistancesIn, P, DNow),
    (DNow = D ->
        update_nbr_dists(Map, P, Q, Q1, DistancesIn, Distances1)
    ;
        Distances1 = DistancesIn,
        Q1 = Q),
    update_dists(Map, Q1, Distances1, DistancesOut).

point_distances(Map, Start, Distances) :-
    init_queue_and_dists(Map, Start, Q, Distances0),
    update_dists(Map, Q, Distances0, Distances).

init_queue_and_dists(Map, Start, Q, Distances) :-
    map_size(Map, Size),
    inf_value(Inf),
    matrix_size_filled(Distances0_, Size, Inf),
    set_matrix_value(Distances0_, Start, 0, Distances),
    Q = [(Start,0)].

%------------------------------
% trail output
%------------------------------
point_char(Trail, Y, X, Char) :- member(((X,Y),Char), Trail), !; Char = '.'.

write_row(Trail, Width, Y, Chars) :-
    Last is Width-1,
    numlist(0, Last, Xs),
    maplist(point_char(Trail, Y), Xs, Chars).

write_trail((W,H), Trail) :-
    Last is H-1,
    numlist(0,Last,Ys),
    maplist(write_row(Trail,W), Ys, Map),
    maplist(writeln, Map).

