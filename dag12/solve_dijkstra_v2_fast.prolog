:- use_module(library(yall)).
:- use_module(library(heaps)).
%:- use_module(library(assoc)).
:- use_module(library(apply)).

inf_dist(10000000).

point(Map,(X,Y),H) :- nth0(Y, Map, Row), nth0(X, Row, H).

height(Map,P,H) :- point(Map,P,H).

map_size(Map, (W,H)) :- length(Map, H), nth0(0, Map, Row), length(Row, W).

can_reach(H0, H1) :- H1 =< H0+1, !; is_end_height(H1), H0 is H1-2. % can reach E from both 'y' and 'z'

can_reachp_(Map, P0, P1) :- height(Map, P0, H0), height(Map, P1, H1), can_reach(H0, H1).

% for solving part 1
%can_reachp(Map, P0, P1) :- can_reachp_(Map, P0, P1).

% for solving part 2
can_reachp(Map, P0, P1) :- can_reachp_(Map, P1, P0).

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

init_distance_queue(Size, Q) :-
    all_pairs(Size, Points),
    empty_heap(Heap),
    Q = queue(Heap,Points).

init_distance_map(Size, Dists) :-
    inf_dist(DInf),
    filled_matrix(Size,DInf,Dists).

empty_queue(queue(H,[])) :- empty_heap(H).

get_from_queue(Q0, Dist, P, Q1) :-
    Q0 = queue(Heap,InfPoints),
    (get_from_heap(Heap, Dist, P, Heap1) ->
        Q1 = queue(Heap1, InfPoints)
    ;
        InfPoints = [P|InfPoints1],
        inf_dist(Dist),
        Q1 = queue(Heap,InfPoints1)
    ).

next_unburned(Map, Queue, DistMapIn, DistMapOut) :-
    (empty_queue(Queue) ->
        % we're done
        DistMapIn = DistMapOut
    ;
        get_from_queue(Queue, Dist, P, Queue1),
        neighbours(Map, P, Nbrs), % part 1
        Dist1 is Dist + 1,
        foldl(update_dist(Dist1), Nbrs, (Queue1,DistMapIn), (Queue2,DistMapIn2)),
        next_unburned(Map, Queue2, DistMapIn2, DistMapOut)
    ).

update_dist(DNew, P, (Queue0, DistMap0), (Queue1, DistMap1)) :-
    matrix_value(DistMap0,P, DOld),
    (DNew < DOld ->
        Queue0 = queue(Heap0,Pts),
        set_matrix_value(DistMap0,P, DNew, DistMap1),
        update_heap_pk(DNew, P, Heap0, Heap1, _),
        Queue1 = queue(Heap1,Pts)
    ;
        Queue1 = Queue0, DistMap1 = DistMap0
    ).

set_dist(DNew, P, (Queue0, DistMap0), (Queue1, DistMap1)) :-
    Queue0 = queue(Heap0,InfPoints),
    set_matrix_value(DistMap0,P, DNew, DistMap1),
    update_heap_pk(DNew, P, Heap0, Heap1, _),
    delete(InfPoints,P,Pts1),  % here we delete from InfPoints, we don't in update_dist() since it's slow and not needed
    Queue1 = queue(Heap1,Pts1).

distances(Map, Start, Distances) :-
    setup(Map, Start, Queue, DistMap),
    next_unburned(Map, Queue, DistMap, Distances).

setup(Map, Start, Queue, DistMap) :-
    map_size(Map, Size),
    init_distance_queue(Size, Queue0),
    init_distance_map(Size, DistMap0),
    set_dist(0, Start, (Queue0,DistMap0), (Queue,DistMap)).

trace_path(Map, Start, End, DistMap, Path) :-
    trace_path_(Map, Start, End, DistMap, Path1),
    append([[(End,'E')], Path1], Path2),
    reverse(Path,Path2).

trace_path_(Map, Start, P, DistMap, Path) :-
    (Start = P ->
        Path = []
    ;
        matrix_value(DistMap,P,DP),
        map_size(DistMap, (W,H)),
        DP1 is DP-1,
        (move_r(W,P,PN), can_reachp(Map,PN,P), matrix_value(DistMap,PN,DP1) ->
            Path = [(PN, '<') | Path1]
        ;
            (move_d(H,P,PN), can_reachp(Map,PN,P), matrix_value(DistMap,PN,DP1) ->
                Path = [(PN, '^') | Path1]
            ;
                (move_l(P,PN), can_reachp(Map,PN,P), matrix_value(DistMap,PN,DP1) ->
                    Path = [(PN, '>') | Path1]
                ;
                    (move_u(P,PN), can_reachp(Map,PN,P), matrix_value(DistMap,PN,DP1) ->
                        Path = [(PN, 'v') | Path1]
                    ;
                        fail
                    )
                )
            )
        ),
        trace_path_(Map, Start, PN, DistMap, Path1)
    ).

%------------------------------
% solve
%------------------------------

% remember to change can_reachp for part 1/2.

solve(Length) :-
    read_input('input',M),
    is_start(M, Start),
    is_end(M, End),
    distances(M,Start,D), % distances to Start
    trace_path(M,Start,End,D,Path),
    length(Path,L),
    Length is L-1.

solve2(Length) :-
    read_input('input',M),
    is_end(M, End),
    distances(M,End,D), % distances to End
    find_closest_a_point(M,D,P,_),
    trace_path(M,End,P,D,Path), % trace reverse
    length(Path,L),
    Length is L-1.

find_closest_a_point(Map,DistMap,PMin,DistMin) :-
    map_size(Map, Size),
    all_pairs(Size, Points),
    include({Map}/[P]>>height(Map,P,0), Points, APoints),
    maplist(point_dist_pair(DistMap), APoints, APointDists),
    sort(2, @=<, APointDists, [PMin-DistMin|_]).

point_dist_pair(DistMap,P,P-Dist) :- matrix_value(DistMap,P,Dist).

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

update_heap_pk(Prio, Key, Heap0, Heap1, Added) :-
    (delete_from_heap(Heap0, _, Key, H1) ->
        Added = false
    ;
        H1 = Heap0,
        Added = true
    ),
    add_to_heap(H1, Prio, Key, Heap1).

matrix((Width,Height),Matrix) :-
    length(Matrix,Height),
    maplist({Width}/[Row]>>length(Row,Width), Matrix).

filled_matrix(Size,Value,Matrix) :-
    matrix(Size, Matrix),
    maplist(maplist(=(Value)),Matrix).

set_list_value([H0|T0],N,V,[H1|T1]) :-
    (N = 0 ->
        H1 = V, T1 = T0
    ;
        H1 = H0,
        N1 is N-1,
        set_list_value(T0,N1,V,T1)
    ).

set_matrix_value(M0,(X,Y),V,M1) :-
    nth0(Y,M0,Row0),
    set_list_value(Row0,X,V,Row1),
    set_list_value(M0,Y,Row1,M1).

matrix_value(M,(X,Y),Value) :-
    nth0(Y, M, Row), nth0(X, Row, Value).

all_pairs((W,H), Pairs) :-
    W0 is W-1, H0 is H-1,
    findall((X,Y), (between(0,W0,X), between(0,H0,Y)), Pairs).
