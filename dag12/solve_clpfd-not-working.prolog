:- use_module(library(yall)).
:- use_module(library(clpfd)).
:- [read_map].
:- [matrix].

height(Map,P,H) :- matrix_value(Map,P,H).

inf_value(100000).

map_size(Map, (W,H)) :- length(Map, H), nth0(0, Map, Row), length(Row, W).

can_reach(H0, H1) :- H1 =< H0+1, ! ; is_end_height(H1), H0 is H1-2. % can reach E from both 'y' and 'z'

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

clp_min_list([H|T],Min) :- foldl([A,B,C]>>(C #= min(A,B)), T, H, Min).
clp_min_list([],_) :- throw(list_must_not_be_empty).

neighbours_min_dist(Map, Dists, P, NbrMinDist) :-
    neighbours(Map, P, Ns),
    maplist(matrix_value(Dists), Ns, NDists),
    clp_min_list(NDists, NbrMinDist).

one_away_from_nbrs(Map, Dists, P) :-
    inf_value(Inf),
    neighbours_min_dist(Map, Dists, P, NbrMinDist),
    matrix_value(Dists, P, PDist),
    PDist #= min(NbrMinDist+1, Inf).

all_but_start_one_away_from_nbrs(Map, Start, Dists) :-
    map_size(Map, Size),
    all_points(Size, AllPoints),
    delete(AllPoints, Start, AllPointsButStart),
    maplist(one_away_from_nbrs(Map, Dists), AllPointsButStart).

distance_matrix_size(Dists, Size) :-
    matrix_size(Dists, Size),
    inf_value(Inf),
    maplist({Inf}/[X]>>(X ins 1..Inf),Dists).

distance_contraints(Map, Dists) :-
    map_size(Map, Size),
    is_start(Map, Start),
    distance_matrix_size(Dists, Size),
    all_but_start_one_away_from_nbrs(Map, Start, Dists),
    matrix_value(Dists, Start, StartDist),
    StartDist #= 0.

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

%----------------------------
% useful functions
%----------------------------
all_points((W,H), Points) :-
    W0 is W-1, H0 is H-1,
    findall((X,Y), (between(0,W0,X), between(0,H0,Y)), Points).
