:- use_module(library(clpfd)). % clpdf for transpose()
:- use_module(library(assoc)).
:- use_module(library(apply)).

%-----------------------
% Find visible trees
%-----------------------
visible1(T,V) :- visible1(-1, T, V).
visible1_rev(T, V) :- reverse(T, R), visible1(R, VR), reverse(V, VR).

visible1(_, [], []).
visible1(TMax, [T|Ts], [1|Vs]) :- T > TMax, visible1(T, Ts, Vs).
visible1(TMax, [T|Ts], [0|Vs]) :- T =< TMax, visible1(TMax, Ts, Vs).

%-----------------------
% Find visibility distance
%-----------------------
viewlen1(T, L) :- init_visible_length(0,9,0,Map), viewlen1(Map, T, L, 0).
viewlen1_rev(T, V) :- reverse(T, R), viewlen1(R, VR), reverse(V, VR).

set_assoc(Value,Key,AIn,AOut) :- put_assoc(Key,AIn,Value,AOut).

set_visible_length(H, Map, Pos, MapOut) :-
    findall(I, between(0,H,I), HList),
    foldl(set_assoc(Pos), HList, Map, MapOut).

init_visible_length(Start, End, Value, Map) :-
    findall(I- Value, between(Start,End,I), L),
    list_to_assoc(L, Map).

viewlen1(_, [], [], _).
viewlen1(Map, [T|Ts], [Dist|Dists], Pos) :-
    get_assoc(T, Map, BlockingTreePos),
    Dist is Pos - BlockingTreePos,
    Pos1 is Pos + 1,
    set_visible_length(T, Map, Pos, Map1),
    viewlen1(Map1, Ts, Dists, Pos1).
%-----------------------

visible(T, V) :-
    maplist(visible1, T, V1),
    maplist(visible1_rev, T, V2),
    transpose(T, TT),
    maplist(visible1, TT, V3T),
    maplist(visible1_rev, TT, V4T),
    transpose(V3T, V3),
    transpose(V4T, V4),
    maplist(maplist(or), V1, V2, V3, V4, V).

or(0,0,0,0,0) :- !.
or(_,_,_,_,1).

scenic_value(T, V) :-
    maplist(viewlen1, T, V1),
    maplist(viewlen1_rev, T, V2),
    transpose(T, TT),
    maplist(viewlen1, TT, V3T),
    maplist(viewlen1_rev, TT, V4T),
    transpose(V3T, V3),
    transpose(V4T, V4),
    maplist(maplist(mult), V1, V2, V3, V4, V).

mult(A,B,C,D,Prod) :- Prod is A*B*C*D.

input(File, LinesAsNumbers) :-
    setup_call_cleanup(open(File, read, In),
       stream_lines(In, LinesAsNumbers),
       close(In)).

stream_lines(In, LinesAsNumbers) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", Lines),
    delete(Lines, "", NonEmptyLines),
    maplist(string_numbers, NonEmptyLines, LinesAsNumbers).

string_numbers(S,Ns) :-
    string_chars(S,Cs),
    maplist(atom_string, Cs, CStrings),
    maplist(number_string, Ns, CStrings).

% Antal synliga från utsidan
solve(NumVisible) :-
    input('input', T), visible(T,V), flatten(V, VFlat), sum_list(VFlat,NumVisible).

% Högsta "scenic value"
solve2(Max) :-
    input('input', T), scenic_value(T,V), flatten(V, VFlat), max_list(VFlat,Max).
