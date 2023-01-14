:- use_module(library(assoc)).

solve1(File, N) :-
    read_input(File, Ws),
    walls_pts(Ws,Ps),
    walls_ymax(Ws, Ym),
    points_assoc(Ps, As),
    drop_sand_v1(Ym, As, As1),
    assoc_length(As, L0),
    assoc_length(As1, L1),
    N is L1-L0-1.

solve2(File, N) :- solve2(File, N, _).

solve2(File, N, As1) :-
    read_input(File, Ws),
    walls_pts(Ws,Ps),
    walls_ymax(Ws, Ym_),
    Ym is Ym_+1, % floor at max+2, so sand falls to max+1
    points_assoc(Ps, As),
    drop_sand_v2(Ym, As, As1),
    assoc_length(As, L0),
    assoc_length(As1, L1),
    N is L1-L0.

%-----------------------------------------
% Assoc
%-----------------------------------------
pt_pair(P, P-1).

points_assoc(Ps, A) :-
    sort(Ps, Sorted),
    maplist(pt_pair, Sorted, PVs),
    ord_list_to_assoc(PVs, A).

assoc_length(A, Len) :-
    assoc_to_list(A, L),
    length(L, Len).

%-----------------------------------------
% Print
%-----------------------------------------
print(_, Y, Y, _, _).
print(Solid, Y, Ymax, X0, X1) :-
    numlist(X0, X1, Xs),
    maplist(yx_char(Solid, Y), Xs, Chars),
    string_chars(S, Chars),
    writeln(S),
    Y1 is Y+1, 
    print(Solid, Y1, Ymax, X0, X1).

yx_char(Solid, Y, X, C) :- (get_assoc(p(X,Y), Solid, _) -> C = '#';  C = '.').

%-----------------------------------------
% Drop sand
%-----------------------------------------
% v1 stop when first sand falls past Ymax to infinity
drop_sand_v1(Ymax, Solid0, Solid1) :-
    drop_one(Ymax, Solid0, Solid0_, Pstop),
    (   Pstop = p(_, Ymax)
    ->  Solid1 = Solid0_
    ;   drop_sand_v1(Ymax, Solid0_, Solid1)).

% v2 floor at Ymax, stop when sand reaches the source (500,0)
drop_sand_v2(Ymax, Solid0, Solid1) :-
    drop_one(Ymax, Solid0, Solid0_, Pstop),
    (   Pstop = p(500, 0)
    ->  Solid1 = Solid0_
    ;   drop_sand_v2(Ymax, Solid0_, Solid1)).

drop_one(Ymax, Solid0, Solid1, Pstop) :-
    fall(Ymax, Solid0, p(500,0), Pstop),
    put_assoc(Pstop, Solid0, 1, Solid1).

%-----------------------------------------
% Falling
%-----------------------------------------
down(  S, p(X,Y), p(X ,Y1)) :- Y1 is Y+1,            \+ get_assoc(p(X,Y1), S, _).
down_l(S, p(X,Y), p(X1,Y1)) :- Y1 is Y+1, X1 is X-1, \+ get_assoc(p(X1,Y1), S, _).
down_r(S, p(X,Y), p(X1,Y1)) :- Y1 is Y+1, X1 is X+1, \+ get_assoc(p(X1,Y1), S, _).

fall_one(Solid, P0, P1) :-
    (   down(Solid, P0, P1), !
    ;   down_l(Solid, P0, P1), !
    ;   down_r(Solid, P0, P1), !
    ;   P1 = P0).

fall(Ymax, _, p(X,Ymax), p(X,Ymax)).
fall(Ymax, Solid, P0, P1) :-
    fall_one(Solid, P0, P0_),
    (   P0 = P0_ -> P1 = P0
                 ;  fall(Ymax, Solid, P0_, P1)).

%-----------------------------------------
% Wall, lines and points
%-----------------------------------------
point_x(p(X,_), X).
point_y(p(_,Y), Y).

xy_point(X, Y, p(X,Y)).
yx_point(Y, X, p(X,Y)).

wall_x(w(Ps), Xs) :- maplist(point_x, Ps, Xs).
wall_y(w(Ps), Ys) :- maplist(point_y, Ps, Ys).

walls_area(Ws, (X0,Y0), (X1,Y1)) :-
    maplist(wall_x, Ws, Xs0),   flatten(Xs0, Xs),
    maplist(wall_y, Ws, Ys0),   flatten(Ys0, Ys),
    min_list(Xs, X0), min_list(Ys, Y0),
    max_list(Xs, X1), max_list(Ys, Y1).

walls_ymax(Ws, Ymax) :- walls_area(Ws, _, (_,Ymax)).

walls_pts(Walls, Points) :- maplist(wall_pts, Walls, Pts0), flatten(Pts0, Points).

% All points on wall. Gives duplicate points in joints
wall_pts(w(Ps), Points) :- wall_pts_(Ps, Points).

wall_pts_([], []).
wall_pts_([_], []).
wall_pts_([P0,P1|Ps], OnWall) :-
    P0 = p(X0,Y0),
    P1 = p(X1,Y1),
    (   X0 == X1
    ->  line_pts(X0, Y0-Y1, OnLine)
    ;   line_pts(X0-X1, Y0, OnLine)),
    wall_pts_([P1|Ps], OnWall0),
    append(OnLine, OnWall0, OnWall).

line_pts(X0-X1, Y, Ps) :- numlist_(X0, X1, L), maplist(yx_point(Y), L, Ps).
line_pts(X, Y0-Y1, Ps) :- numlist_(Y0, Y1, L), maplist(xy_point(X), L, Ps).

numlist_(A,B,L) :-
    (   A > B
    ->  numlist(B, A, L0), reverse(L0, L)
    ;   numlist(A, B, L)).

%-----------------------------------------
% read input
%-----------------------------------------
:- use_module(library(dcg/basics)).
%:- use_module(library(dcg/high_order)). % sequence from dcg/high_order hangs
:- use_module(library(pio)).

point(p(X,Y)) --> integer(X), ",", integer(Y).

wall(w(Ps)) --> sequence(point, " -> ", Ps), eol.

read_input(File, Walls) :- phrase_from_file(sequence(wall, Walls), File).

%-----------------------------------------
% DCG helpers
%-----------------------------------------
sequence(G, [R]) --> call(G, R).
sequence(G, [R|Rs]) --> call(G, R), sequence(G, Rs).

sequence(G, _, [R]) --> call(G, R).
sequence(G, Sep, [R|Rs]) --> call(G, R), Sep, sequence(G, Sep, Rs).
