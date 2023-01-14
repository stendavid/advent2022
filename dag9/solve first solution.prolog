

move(u, (X,Y1), (X, Y2)) :- Y2 is Y1 + 1.
move(d, (X,Y1), (X, Y2)) :- Y2 is Y1 - 1.
move(l, (X1,Y), (X2, Y)) :- X2 is X1 - 1.
move(r, (X1,Y), (X2, Y)) :- X2 is X1 + 1.

follow1(H, T, TOut) :- H > T, TOut is T + 1.
follow1(H, T, TOut) :- H < T, TOut is T - 1.
follow1(H, H, H).

follow((Hx,Hy), (Tx, Ty), (Tx, Ty)) :- abs(Tx-Hx) =< 1, abs(Ty-Hy) =< 1.
follow((Hx,Hy), (Tx, Ty), (TxOut, TyOut)) :- (abs(Tx-Hx) > 1; abs(Ty-Hy) > 1),
    follow1(Hx, Tx, TxOut),
    follow1(Hy, Ty, TyOut).

do_one(C, rope(H, T, Trail), rope(HOut, TOut, [TOut|Trail])) :-
    move(C, H, HOut),
    follow(HOut, T, TOut).

do_command(cmd(_, 0), R, R).
do_command(cmd(C, N), RIn, ROut) :- N > 0,
    do_one(C, RIn, ROut1),
    N1 is N - 1,
    do_command(cmd(C,N1), ROut1, ROut).

do_commands([], R, R).
do_commands([C|Cs], RIn, ROut) :- do_command(C, RIn, ROut1), do_commands(Cs, ROut1, ROut).

start_position(rope((0,0), (0,0), [(0,0)])).

solve(NUniquePos) :-
    read_commands('input', Commands),
    start_position(R),
    do_commands(Commands, R, ROut),
    ROut = rope(_,_,Trail),
    list_to_set(Trail, S),
    length(S, NUniquePos).

read_commands(File, Commands) :-
    read_lines(File, Lines),
    delete(Lines, "", NonEmptyLines),
    maplist(read_command, NonEmptyLines, Commands).

read_command(Line, cmd(C,N)) :-
    split_string(Line, " ", "", [CUpper,NStr]),
    number_string(N, NStr),
    downcase_atom(CUpper, C).

read_lines(File, Lines) :-
    setup_call_cleanup(open(File, read, In),
       stream_lines(In, Lines),
       close(In)).

stream_lines(In, Lines) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", Lines).
