:- dynamic edge/2.

%------------------------------
% Build edge/2 from map
%------------------------------
assert_edges(Map) :-
    retractall(edge(_,_)),
    map_edges(Map, Es),
    maplist(assertz, Es).

%------------------------------
% Return a list [edge(P1,P2),...] for all points to their reachable neighbours
%------------------------------
map_edges(Map, Es) :-
    map_size(Map, Size),
    all_pairs(Size, Points),
    maplist(edges_to_nbrs(Map), Points, Es0),
    flatten(Es0, Es).

edges_to_nbrs(Map, P, Es) :-
    findneighbours(Map, P, Ns),
    maplist(p_n_edge(P), Ns, Es).

p_n_edge(P,N,edge(P,N)).

%------------------------------
% Neighbours
%------------------------------
can_reach(Map, Pfrom, Pto) :-
    height(Map, Pfrom, H0),
    height(Map, Pto, H1),
    (H1 =< H0+1, !; is_end_height(H1), H0 is H1-2). % can reach E from both 'y' and 'z'

on_map((W,H), (X,Y)) :- X >= 0, X < W, Y >= 0, Y < H.

findneighbours(Map, P, Ns) :-
    P = (X,Y),
    map_size(Map, Size),
    Rx is X+1, Uy is Y-1,
    Lx is X-1, Dy is Y+1,
    Ns0 = [(Rx,Y), (Lx,Y), (X,Dy), (X,Uy)],
    include(can_reach(Map, P), Ns0, Ns1),
    include(on_map(Size), Ns1, Ns).

%------------------------------
% Map functions
%------------------------------
map_size(Map, (W,H)) :- length(Map, H), nth0(0, Map, Row), length(Row, W).

% height
%  S -> 100, E -> 200, a-z -> 0-25
height(Map,(X,Y),H) :- nth0(Y, Map, Row), nth0(X, Row, H).

char_height('S', 100) :- !.
char_height('E', 26) :- !.  % E has the same hight as 'z' (25) but we need to see the difference
char_height(Char, H) :- char_code(Char, Code), H is Code-97.

is_start(Map, P) :- height(Map, P, 100).
is_end(Map, P) :- height(Map, P, 26).   % matches char_height('E')

is_end_height(26).

%------------------------------
% Read map from file
%------------------------------
read_map(File, Map) :-
    read_lines(File, Lines),
    delete(Lines, "", NonEmptyLines),
    maplist(string_chars, NonEmptyLines, MapChar),
    maplist(maplist(char_height), MapChar, Map).

read_lines(File, Lines) :-
    setup_call_cleanup(open(File, read, In),
       stream_lines(In, Lines),
       close(In)).

stream_lines(In, Lines) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", Lines).

%------------------------------
% Helpers
%------------------------------
all_pairs((W,H), Pairs) :-
    W0 is W-1, H0 is H-1,
    findall((X,Y), (between(0,W0,X), between(0,H0,Y)), Pairs).
