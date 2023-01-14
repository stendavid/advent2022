:- dynamic edge/2.

%:- [map].
:- [mapx].

solve(File, L) :-
    read_map(File, Map),
    map_size(Map, Size),
    is_start(Map, Start),
    is_end(Map, End),
    abolish_all_tables, % needed since we change 'edge', maybe incremental could be used instead 
    %assert_edges(Map),
    %path(Start, End, L).
    idx_point(Size, IStart, Start),
    idx_point(Size, IEnd, End),
    map_idxedges(Map, Es),
    maplist(assertz, Es),
    path(IStart, IEnd, L).

map_idxedges(Map, IdxEdges) :-
    map_size(Map, Size),
    map_edges(Map, Es),
    maplist(edge_idxedge(Size), Es, IdxEdges).

edge_idxedge(Size,edge(P1,P2), edge(I1,I2)) :-
    idx_point(Size, I1, P1),
    idx_point(Size, I2, P2).
    
idx_point((W,_),I,(X,Y)) :-
    (   var(I)
    ->  I is Y*W + X
    ;   X is I mod W,
        Y is I div W).

%------------------------------
% Memoized shortest path
%------------------------------
% abolish is needed in SWI to keep 'path' tabled after reload
%:- abolish(path/3).
%:- table path(_,_,min) as subsumptive.
:- table path(_,_,po('@<'/2)).

%-- Version that stores the next node in the table, and traces the path in a separate function
path(X, Y, L-Z) :- path(Z, Y, LP0), edge(X, Z), LP0 = L0-_, L is L0+1.
path(X, X, 0-X).

trace_path(X, X, [X]) :- !.
trace_path(X, Y, [X|PNxt]) :-
    path(X, Y, LenNxt),
    LenNxt = _-Nxt,
    trace_path(Nxt, Y, PNxt).

%-- Version that stores the whole path in the table
%path(X, X, 0-[X]).
%path(X, Y, L-[X|P0]) :- path(Z, Y, LP0), edge(X, Z), LP0 = L0-P0, L is L0+1.

%-- Version with debug log
% path(X, X, 0). %format("path((~w),(~w),0)~n", [X,X]),
% path(X, Y, L) :-
%     format("ENTER       path((~w)->(~w),~w)~n", [X,Y,L]),
%     path(Z, Y, L0),
%     format("  FOUND     path((~w)->(~w),~w)~n", [Z,Y,L0]),
%     % (   path(Z, Y, L0)
%     % ->  format("  FOUND     path((~w)->(~w),~w)~n", [Z,Y,L0])
%     % ;   format("  NOT FOUND path((~w)->(~w),_)~n", [Z,Y]),
%     %     fail),
%     edge(X, Z),
%     %format("  CHECK     path((~w)->(~w))          edge((~w)->(~w))~n", [Z,Y,X,Z]),
%     L is L0+1,
%     format("EXIT        path((~w)->(~w),~w)~n", [X,Y,L]).

min3(A,B,C) :- C is min(A,B).