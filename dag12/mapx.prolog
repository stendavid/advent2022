% XSB version
:- dynamic edge/2.

:- [lists].
%:- import maplist/2, maplist/3 from swi.
:- import between/3, flatten/2 from basics.
:- import include/3 from swi.

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
    %delete(Lines, "", NonEmptyLines),
    my_delete(Lines, [], MapChar),
    %maplist(string_chars, NonEmptyLines, MapChar),
    maplist(maplist(char_height), MapChar, Map).

% read_lines(File, Lines) :-
%     setup_call_cleanup(open(File, read, In),
%        stream_lines(In, Lines),
%        close(In)).

read_lines(File, Lines) :-
    open(File, read, In),
    call_cleanup(
        stream_lines(In, Lines),
        close(In)).

stream_lines(In, Lines) :-
    %read_string(In, _, Str),
    %split_string(Str, "\n", "", Lines).
    my_read_string(In, Str),
    my_split(Str, '\n', Lines).

my_read_string(In, S) :-
    get_char(In, C),
    (   C = end_of_file
    ->  S = []
    ;   my_read_string(In, S0),
        S = [C|S0]).

my_split([], _, [[]]).
my_split([C|Cs], C, [[]|Ls]) :- my_split(Cs, C, Ls), !.
my_split([C|Cs], Sep, [[C|Cs0]|Ls]) :- my_split(Cs, Sep, [Cs0|Ls]), !.

my_delete([], _, []).
my_delete([Elem|Tail], Del, Result) :-
    (   \+ Elem \= Del
    ->  my_delete(Tail, Del, Result)
    ;   Result = [Elem|Rest],
        my_delete(Tail, Del, Rest)
    ).

%------------------------------
% Helpers
%------------------------------
all_pairs((W,H), Pairs) :-
    W0 is W-1, H0 is H-1,
    findall((X,Y), (between(0,W0,X), between(0,H0,Y)), Pairs).

% maplist
maplist(Goal, List) :-
    maplist_(List, Goal).

maplist_([], _).
maplist_([Elem|Tail], Goal) :-
    call(Goal, Elem),
    maplist_(Tail, Goal).

maplist(Goal, List1, List2) :-
    maplist_(List1, List2, Goal).

maplist_([], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], Goal) :-
    call(Goal, Elem1, Elem2),
    maplist_(Tail1, Tail2, Goal).

maplist(Goal, List1, List2, List3) :-
    maplist_(List1, List2, List3, Goal).

maplist_([], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], Goal) :-
    call(Goal, Elem1, Elem2, Elem3),
    maplist_(Tail1, Tail2, Tail3, Goal).

maplist(Goal, List1, List2, List3, List4) :-
    maplist_(List1, List2, List3, List4, Goal).

maplist_([], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], Goal) :-
    call(Goal, Elem1, Elem2, Elem3, Elem4),
    maplist_(Tail1, Tail2, Tail3, Tail4, Goal).