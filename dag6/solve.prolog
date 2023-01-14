:- use_module(library(lists)).
%:- use_module(library(dcg/basics)).
%:- use_module(library(pio)).
:- use_module(library(readutil)).
:- use_module(library(clpfd)).

distinct([]).
distinct([_]).
distinct([A|Rest]) :- \+ member(A, Rest), distinct(Rest).

prefixn(0, [], _).
prefixn(N, [L|Prefix], [L|List]) :- N #> 0, M #= N-1, prefixn(M, Prefix, List).

find(List, P, Marker, MarkerLen) :-
    append(First, Second, List),
    prefixn(MarkerLen, Marker, Second),
    distinct(Marker),
    length(First, Pos),
    P #= Pos + MarkerLen.

solve(Pos, Marker, MarkerLen) :-
    read_file_to_codes('input', CodeList, []),
    find(CodeList, Pos, Marker, MarkerLen).
