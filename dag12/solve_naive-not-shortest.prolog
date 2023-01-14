:- use_module(library(yall)).

point(Map,(X,Y),H) :- nth0(Y, Map, Row), nth0(X, Row, H).

height(Map,P,H) :- point(Map,P,H).

map_size(Map, (W,H)) :- length(Map, H), nth0(0, Map, Row), length(Row, W).

move('^', (_,_), (X0,Y0), (X0,Y1)) :- Y0 > 0, Y1 is Y0-1.
move('v', (_,H), (X0,Y0), (X0,Y1)) :- Y0 < H-1, Y1 is Y0+1.
move('<', (_,_), (X0,Y0), (X1,Y0)) :- X0 > 0, X1 is X0-1.
move('>', (W,_), (X0,Y0), (X1,Y0)) :- X0 < W-1, X1 is X0+1.

can_reach(H0, H1) :- H1 =< H0+1, ! ; is_end_height(H1), H0 is H1-2. % can reach E from both 'y' and 'z'

next(Map, Size, Visited, P0, P1, Direction) :-
    height(Map, P0, H0),
    move(Direction, Size, P0, P1),
    height(Map, P1, H1),
    can_reach(H0, H1),
    \+ member(P1, Visited).

run(Map, Size, Visited, P0, [(P0,Direction)|Trail]) :-
    next(Map, Size, Visited, P0, P1, Direction),
    (is_end(Map, P1) ->
        Trail = []
    ;
        run(Map, Size, [P1|Visited], P1, Trail)
    ).

path(Map, Length, Trail) :-
    map_size(Map, Size),
    is_start(Map, Start),
    run(Map, Size, [], Start, Trail),
    length(Trail, Length).

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