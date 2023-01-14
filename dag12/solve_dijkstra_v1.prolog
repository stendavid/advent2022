:- use_module(library(yall)).
:- use_module(library(heaps)).
:- use_module(library(assoc)).
:- use_module(library(apply)).

inf_dist(10000000).

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

init_distance_heap(Size, Heap) :-
    all_pairs(Size, Points),
    empty_heap(Heap0),
    inf_dist(DInf),
    foldl(add_to_heap_pk(DInf), Points, Heap0, Heap).

next_unburned(_, Unburned, []) :- empty_heap(Unburned).
next_unburned(Map, Unburned, [dist(Distance, P)|Distances0]) :-
    get_from_heap(Unburned, Distance, P, Unburned1),
    neighbours(Map, P, Nbrs),
    Dist1 is Distance + 1,
    foldl(update_heap_pk(Dist1), Nbrs, Unburned1, Unburned2),
    next_unburned(Map, Unburned2, Distances0).

distances(Map, Distances) :-
    map_size(Map, Size),
    is_start(Map, Start),
    init_distance_heap(Size, Unburned0),
    update_heap_pk(0, Start, Unburned0, Unburned),
    next_unburned(Map, Unburned, Distances).

distance_of(P, [dist(D,P)|_], D) :- !.
distance_of(P, [dist(_,Q)|T], D) :- dif(P,Q), distance_of(P, T, D).

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

%------------------------------
% Read height map: S -> 100, E -> 200, a-z -> 0-25
%------------------------------
read_input(File, Map) :-
    read_lines(File, Lines),
    delete(Lines, "", NonEmptyLines),
    maplist(string_chars, NonEmptyLines, MapChar),
    maplist(maplist(map_height), MapChar, Map).

map_height('S', 100) :- !.
map_height('E', 26) :- !.  % E has the same hight as 'z' (25) but we need to see the difference
map_height(Char, H) :- char_code(Char, Code), H is Code-97.

is_start(Map, P) :- height(Map, P, 100).
is_end(Map, P) :- height(Map, P, 26).   % matches map_height('E')

is_end_height(26).

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
add_to_heap_pk(Prio, Key, Heap0, Heap1) :- add_to_heap(Heap0, Prio, Key, Heap1).

update_heap_pk(Prio, Key, Heap0, Heap1) :-
    (delete_from_heap(Heap0, Prio0, Key, H1) ->
        MinPrio is min(Prio, Prio0),
        add_to_heap(H1, MinPrio, Key, Heap1)
    ;
        Heap1 = Heap0
    ).

all_pairs((W,H), Pairs) :-
    W0 is W-1, H0 is H-1,
    findall((X,Y), (between(0,W0,X), between(0,H0,Y)), Pairs).
