% this is way to slow, can solve part 1 in ~30-40 seconds, but not part 2

solve2(File, Size, Freq, X, Y) :-
    read_input(File, Sensors),
    between(0,Size,Y),
    between(0,Size,X),
    P = point(X,Y),
    (X = 0 -> write("Line "), writeln(Y) ; true),
    \+ any(point_in_range(P), Sensors),
    Freq is X*4000000+Y.

solve1(File, Y, N) :-
    read_input(File, Sensors),
    min_x(Sensors, Xmin),
    max_x(Sensors, Xmax),
    maplist(sensor_beacon, Sensors, Beacons),
    count_impossible(Sensors, Beacons, Y, Xmin, Xmax, 0, N).

%----------------------------------
% Count points that can't be a beacon
%----------------------------------
point_impossible(P, Ss, Bs) :- any(point_in_range(P), Ss), \+ memberchk(P, Bs).

count_impossible(Ss, Bs, Y, X, X, Nacc, N) :- (point_impossible(point(X,Y), Ss, Bs) -> N = Nacc+1 ; N = Nacc).
count_impossible(Ss, Bs, Y, X0, X1, Nacc, N) :-
    X0 < X1, 
    (point_impossible(point(X0,Y), Ss, Bs) -> Nacc_ is Nacc+1 ; Nacc_ = Nacc),
    X0_ is X0 + 1,
    count_impossible(Ss, Bs, Y, X0_, X1, Nacc_, N).

% For printing
point_char(Ss,Y,X,C) :-
    P = point(X,Y),
    (   any(beacon_sensor(P), Ss)   -> C = 'B'
    ;   any(position_sensor(P), Ss) -> C = 'S'
    ;   any(point_in_range(P), Ss)  -> C = '#'
    ;                                  C = '.').

%----------------------------------
% Range check
%----------------------------------
manhattan(P1, P2, D) :-
    %verify_point(P1), verify_point(P2),
    P1 = point(X1,Y1), P2 = point(X2,Y2),
    D is abs(X1-X2) + abs(Y1-Y2).

point_in_range(point(Xp,Yp), sensor(point(Xs,Ys),_,Db)) :-
    Dp is abs(Xp-Xs) + abs(Yp-Ys),
    Dp =< Db.

% For checking size of input
sensor_min_x(S, V) :- verify_sensor(S,P,B), P = point(Px,_), manhattan(P,B,D), V is Px-D.
sensor_max_x(S, V) :- verify_sensor(S,P,B), P = point(Px,_), manhattan(P,B,D), V is Px+D.

min_x(Ss, V) :- maplist(sensor_min_x, Ss, Vs), min_list(Vs, V).
max_x(Ss, V) :- maplist(sensor_max_x, Ss, Vs), max_list(Vs, V).

%----------------------------------
% Points and sensors
%----------------------------------
verify_point(P) :- ((var(P); P = point(_,_)) -> true; type_error(point(...), P)).
verify_point(P,X,Y) :- (P = point(X,Y) -> true; type_error(point(...), P)).

verify_sensor(S) :- (var(S); S = sensor(_,_,_) -> true; type_error(sensor(...), S)).
verify_sensor(S,P,B) :- (S = sensor(P,B,_) -> true; type_error(sensor(...), S)).

beacon_sensor(B, sensor(_,B,_)).
sensor_beacon(sensor(_,B,_), B).
position_sensor(P, sensor(P,_,_)).

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

%-----------------------------------------
% Other helpers
%-----------------------------------------
any(Goal, [I]) :- call(Goal, I), !.
any(Goal, [I|Is]) :- call(Goal, I) -> true ; any(Goal, Is).
