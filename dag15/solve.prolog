
solve1(File, Y, N) :-
    read_input(File, Sensors),
    maplist(sensor_beacon, Sensors, Beacons),
    count_impossible(Sensors, Beacons, Y, N).

solve2(File, Size, Freq, X, Y) :-
    read_input(File, Sensors),
    between(0, Size, Y),
    (0 is Y mod 100000 -> write("Line "), writeln(Y) ; true),
    sensor_ranges_at_y(Y, Sensors, RangeList),
    find_x_between_ranges(RangeList, X),
    Freq is 4000000*X + Y.

find_x_between_ranges([_-R1,_|_], X) :- X is R1+1.

%----------------------------------
% Count points that can't be a beacon
%----------------------------------
count_impossible(Ss, Bs, Y, N) :-
    sensor_ranges_at_y(Y, Ss, RangeList),
    maplist(range_size, RangeList, RangeSizes),
    sum_list(RangeSizes, TotalRangeSize),
    count_beacons_on_y(Y, Bs, NumBeaconsOnY),
    N is TotalRangeSize - NumBeaconsOnY.

count_beacons_on_y(Y, Bs, N) :-
    include(yx_point(Y,_), Bs, BeaconsOnY),
    sort(BeaconsOnY, UniqueBeacons), % remove duplicates
    length(UniqueBeacons, N).

%----------------------------------
% Sensor ranges
%----------------------------------
sensor_ranges_at_y(Y, Sensors, RangeList) :-
    foldl(add_sensor_range_at_y(Y), Sensors, [], RangeList).

add_sensor_range_at_y(Y, Sensor, RangeList0, RangeList1) :-
    (   sensor_range_at_y(Y, Sensor, Range)
    ->  add_range(RangeList0, Range, RangeList1)
    ;   RangeList1 = RangeList0).

sensor_range_at_y(Y, sensor(point(Xs,Ys),_,D), Xmin-Xmax) :-
    H is abs(Y-Ys),
    H =< D,
    Xmin is Xs - (D-H),
    Xmax is Xs + (D-H).

%----------------------------------
% Ranges / sorted range lists
%----------------------------------
add_range([], New, [New]).
add_range([R|Rs0], New, Rs1) :-
    R = R0-R1,
    New = N0-N1,
    (   N0 =< R1+1, N1 >= R0-1
    ->  % new is overlapping or adjacent: merge
        X0 is min(R0,N0),
        X1 is max(R1,N1),
        add_range(Rs0, X0-X1, Rs1)
    ;   N0 < R0
    ->  % new is lower: add before
        Rs1 = [New,R|Rs0]
    ;   % new is higher: add after
        add_range(Rs0, New, Rs1_),
        Rs1 = [R|Rs1_]).

range_size(R0-R1, S) :- S is R1-R0+1.

ranges_contain([R0-R1|Rs], V) :-
    (   R0 =< V, V =< R1
    ->  true
    ;   ranges_contain(Rs, V)).

%----------------------------------
% Manhattan distance
%----------------------------------
manhattan(P1, P2, D) :-
    verify_point(P1), verify_point(P2),
    P1 = point(X1,Y1), P2 = point(X2,Y2),
    D is abs(X1-X2) + abs(Y1-Y2).

%----------------------------------
% Points and sensors
%----------------------------------
verify_point(P) :- ((var(P); P = point(_,_)) -> true; type_error(point(...), P)).

yx_point(Y,X,point(X,Y)).

sensor_beacon(sensor(_,B,_), B).

%----------------------------------
% Read input
%----------------------------------
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

% Sensor at x=489739, y=1144461: closest beacon is at x=-46516, y=554951

point(point(X,Y)) --> "x=", integer(X), ", y=", integer(Y).

sensor(sensor(S,B,D)) --> "Sensor at ", point(S), ": closest beacon is at ", point(B), eol,
                        { manhattan(S,B,D) }.

read_input(File, Sensors) :- phrase_from_file(sequence(sensor, Sensors), File).

%-----------------------------------------
% DCG helpers
%-----------------------------------------
sequence(G, [R]) --> call(G, R).
sequence(G, [R|Rs]) --> call(G, R), sequence(G, Rs).

sequence(G, _, [R]) --> call(G, R).
sequence(G, Sep, [R|Rs]) --> call(G, R), Sep, sequence(G, Sep, Rs).
