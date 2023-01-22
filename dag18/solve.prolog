:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(func)).
:- use_module(library(apply)).

solve1(File, NumOpen) :-
    assert_cube(File),
    count_open_sides(NumOpen). % Test solution=64, Solution=4636

solve2(File, NumOpen) :-
    assert_cube(File),
    count_outside_sides(NumOpen).  % Test solution=58, Solution=2572

%-------------------------------
% Water
%-------------------------------
% abolish is needed in SWI to keep predicate tabled after reload
:- abolish(water/3).
:- table water/3.

water(0,0,0).
water(X,Y,Z) :- 
    \+ cube(X,Y,Z),
    bounds_ext(Min, Max),
    neighbours_in_bounds(Min, Max, X-Y-Z, Ns),
    member(N, Ns),
    water(N).

water(X-Y-Z) :- water(X,Y,Z).

%-------------------------------
% Count sides
%-------------------------------
%  -- Open sides for part 1 --
count_open_sides(NumOpen) :-
    findall(X-Y-Z, cube(X,Y,Z), Cubes),
    maplist(count_open_sides_, Cubes, Ns),
    sum_list(Ns, NumOpen).

count_open_sides_(Cube, NumOpen) :-
    neighbours(Cube, Ns),
    exclude(cube, Ns, Open),
    length(Open, NumOpen).

cube(X-Y-Z) :- cube(X,Y,Z).

% -- Outside sides for part 2 --
count_outside_sides(NumOpen) :-
    findall(X-Y-Z, cube(X,Y,Z), Cubes),
    maplist(count_outside_sides_, Cubes, Ns),
    sum_list(Ns, NumOpen).

count_outside_sides_(Cube, NumOpen) :-
    neighbours(Cube, Ns),
    include(water, Ns, Open), % only water neighbours
    length(Open, NumOpen).

%-------------------------------
% Neighbours of the cube at X,Y,Z
%-------------------------------
neighbours(X-Y-Z, Ns) :-
    Ns = [
        (~ is X-1)-Y         -Z,
        (~ is X+1)-Y         -Z,
        X         -(~ is Y-1)-Z,
        X         -(~ is Y+1)-Z,
        X         -Y         -(~ is Z-1),
        X         -Y         -(~ is Z+1)
    ].

neighbours_in_bounds(Min, Max, Cube, Ns) :-
    neighbours(Cube, Ns0),
    include(in_bounds(Min, Max), Ns0, Ns).

in_bounds(Xmin-Ymin-Zmin, Xmax-Ymax-Zmax, X-Y-Z) :-
    between(Xmin, Xmax, X),
    between(Ymin, Ymax, Y),
    between(Zmin, Zmax, Z).

%-------------------------------
% The cube/3 predicate
%-------------------------------
:- dynamic cube/3.

assert_cube(File) :-
    abolish_all_tables, % table water/3 depends on cube/3
    retractall(cube(_,_,_)),
    read_input(File, Cubes),
    maplist(assertz, Cubes).

%-------------------------------
% Bounds of cube/3
%-------------------------------
% abolish is needed in SWI to keep predicate tabled after reload
:- abolish(bounds_ext/2).
:- table bounds_ext/2. % tabled for caching

bounds_ext(Xmin-Ymin-Zmin, Xmax-Ymax-Zmax) :-
    bounds(Xmin0-Ymin0-Zmin0, Xmax0-Ymax0-Zmax0),
    Xmin is Xmin0-1, Xmax is Xmax0+1,
    Ymin is Ymin0-1, Ymax is Ymax0+1,
    Zmin is Zmin0-1, Zmax is Zmax0+1.

bounds(Xmin-Ymin-Zmin, Xmax-Ymax-Zmax) :-
    findall(V, cube(V,_,_), Xs),
    findall(V, cube(_,V,_), Ys),
    findall(V, cube(_,_,V), Zs),
    min_list(Xs, Xmin), max_list(Xs, Xmax),
    min_list(Ys, Ymin), max_list(Ys, Ymax),
    min_list(Zs, Zmin), max_list(Zs, Zmax).

%-------------------------------
% Input
%-------------------------------
dcg_cube(cube(X,Y,Z)) --> integer(X), ",", integer(Y), ",", integer(Z), eol.
dcg_cubes(Cubes) --> sequence(dcg_cube, Cubes).

read_input(File, Cubes) :- phrase_from_file(dcg_cubes(Cubes), File), !.

%-------------------------------
% DCG helpers
%-------------------------------
sequence(_, []) --> [].
sequence(G, [H|T]) --> call(G,H), sequence(G,T).