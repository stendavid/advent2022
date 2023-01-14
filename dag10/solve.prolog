
run_([], _, _, []).
run_([cmd(noop)|Cs], X, [Add0, Add1], [X1|Xs]) :-
    X1 is X + Add0,
    run_(Cs, X1, [Add1, 0], Xs).

run_([cmd(addx,N)|Cs], X, [Add0, Add1], [X1|Xs]) :-
    X1 is X + Add0,
    run_([cmd(noop)|Cs], X1, [Add1, N], Xs).

run(Commands, XTrail) :- run_(Commands, 1, [0,0], XTrail). 

signal([], [], _).
signal([X|Xs], [Sig|Sigs], N) :- Sig is X * N, N1 is N+1, signal(Xs, Sigs, N1).

nth1_(Values, N, Out) :- nth1(N, Values, Out).

select(Values, Ns, Out) :- maplist(nth1_(Values), Ns, Out).

range(Start,End,L) :- findall(I,between(Start,End,I),L).

draw(X,Pixel,"X") :- abs((Pixel mod 40)-(X mod 40)) <2.
draw(X,Pixel," ") :- abs((Pixel mod 40)-(X mod 40)) >= 2.

take([], _, [], []).
take(List, 0, [], List).
take([L|List], N, [L|Taken], Rest) :- N > 0, N1 is N - 1, take(List, N1, Taken, Rest).

chunks(_,[],[]).
chunks(Size,L,[C|Cs]) :- take(L, Size, C, Rest), chunks(Size, Rest, Cs).

% Sum "signal streangth" at given cycles 
solve(Sum) :-
    read_commands('input',Commands),
    run(Commands,X),
    signal(X, S, 1),
    select(S,[20,60,100,140,180,220],V),
    sum_list(V,Sum). 

% Print drawn pixels
solve2 :-
    read_commands('input',Commands),
    run(Commands,X),
    range(0,239,Pixels),
    maplist(draw,Pixels,X,Screen),
    chunks(40,Screen,Lines),
    maplist(writeln,Lines).

read_commands(File, Commands) :-
    read_lines(File, Lines),
    delete(Lines, "", NonEmptyLines),
    maplist(read_command, NonEmptyLines, Commands).

read_command("noop", cmd(noop)).
read_command(Line, cmd(addx,N)) :-
    split_string(Line, " ", "", ["addx", NStr]),
    number_string(N, NStr).

read_lines(File, Lines) :-
    setup_call_cleanup(open(File, read, In),
       stream_lines(In, Lines),
       close(In)).

stream_lines(In, Lines) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", Lines).
