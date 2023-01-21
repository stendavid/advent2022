%                     128  32
%                   256 |64 |16 8 4 2 1
% walls               1 0 0 0 0 0 0 0 1     257
% floor               1 1 1 1 1 1 1 1 1     511
%                     | <- walls   -> | 
piece(1, [60]).    %  . . . 1 1 1 1 . .

piece(2, [16,      %  . . . . 1 . . . .
          56,      %  . . . 1 1 1 . . .
          16]).    %  . . . . 1 . . . .

piece(3, [8,       %  . . . . . 1 . . .
          8,       %  . . . . . 1 . . .
          56]).    %  . . . 1 1 1 . . .

piece(4, [32,      %  . . . 1 . . . . .
          32,      %  . . . 1 . . . . .
          32,      %  . . . 1 . . . . .
          32]).    %  . . . 1 . . . . .

piece(5, [48,      %  . . . 1 1 . . . .
          48]).    %  . . . 1 1 . . . .

walls(257).
floor([511]).

%----------------------------
% Solve
%----------------------------
solve1_test(N) :-
    wind_test(W), run(2022,W,_,N). %3068

solve1(N) :-
    wind(W), run(2022,W,_,N). %3098

solve2_test(N) :-
    wind_test(W), run(1000000000000,W,_,N). %1514285714288

solve2(N) :-
    wind(W), run(1000000000000,W,_,N). %1525364431487

%----------------------------
% Print rocks
%----------------------------
write_rocks(Rows) :- maplist(write_row, Rows).

write_row(N) :- num_binstr(N,S), writeln(S).

num_binstr(Num, BinStr) :- numlist(0,8,L), reverse(L,Bits), maplist(binchar(Num),Bits,Cs), string_chars(BinStr,Cs).

binchar(Num,Bit,C) :- 0 is 2**Bit /\ Num -> C='.'; C='1'.

%----------------------------
% Run rock fall simulation
%----------------------------

run(Nmax, Wind, RowsOut, H) :-
    floor(Rows0),
    wind_array_length(Wind, WindLen),
    run(Nmax, 0, Wind, 0, cycles(WindLen, 0, []), Rows0, RowsOut, SkippedHeight),
    length(RowsOut, L),
    H is L + SkippedHeight - 1. % current height + skipped cycles - floor

run(Nmax, Nmax, _, _, _, Rows0, Rows0, 0) :- !.
run(Nmax, N, Wind, Wn, CycleData, Rows0, Rows1, SkippedHeight) :-
    (0 is N mod 10000 -> write("--- piece "), writeln(N); true),
    N < Nmax,
    Pn is N mod 5 + 1,
    piece(Pn, Piece),
    fall(Piece, Wind, Wn, Wn_, Rows0, Rows0_),
    N0_ is N+1,
    cycles_check(CycleData, N0_, Wn_, Rows0_, CycleData_, Cycle),
    (Cycle = no ->
        run(Nmax, N0_, Wind, Wn_, CycleData_, Rows0_, Rows1, SkippedHeight)
    ;
        skip_forward(Cycle, Nmax, Rows0_, N0_, N1_, SkippedHeight),
        run(Nmax, N1_, Wind, Wn_, done, Rows0_, Rows1, _)
    ).

skip_forward(sample(_,_,SampleN, SampleH), Nmax, Rows, N0, N1, SkippedHeight) :-
    % Skip forward as many cycles as possible
    length(Rows, Heigth),
    NRocks is N0 - SampleN,            % Number of rocks in one cycle
    NCycles is (Nmax - N0) div NRocks, % Number of cycles we can skip
    SkippedHeight is NCycles * (Heigth - SampleH),  % Height of skipped cycles
    N1 is N0 + NCycles * NRocks,                    % First rock after skipped cycles
    format("Found cycle: skipping to rock ~p for height ~p ~n", [N1, SkippedHeight]).

cycles_check(done, _, _, _, done, no) :- !.
cycles_check(Cycles0, N, Wn, Rows, Cycles1, Found) :-
    Cycles0 = cycles(WindLen, LastWn, Samples),
    (Wn < LastWn + WindLen ->
        % wind list hasn't cycled since last sample yet
        Cycles1 = Cycles0,
        Found = no
    ;
        % wind list has cycled, check for cycle in saved samples
        find_cycle(WindLen, Wn, Rows, Samples, FoundSample) ->
            % found cycle
            FoundSample = sample(SampleWn, _, _, _),
            Cycles1 = Cycles0,
            Found = FoundSample,
            format("Found cycle: ~p - ~p~n", [SampleWn,Wn])
        ;
            % no cycle found, add this position as a sample
            take_until_unreachable(Rows, SampleRows),
            length(Rows, Heigth),
            Sample = sample(Wn, SampleRows, N, Heigth),
            Cycles1 = cycles(WindLen, Wn, [Sample|Samples]),
            Found = no,
            length(SampleRows, L),
            format("Checked at ~p, no cycle found. Take ~p rows sample~n", [Wn,L])
    ).

find_cycle(WindLen, Wn, Rows, [Sample|Samples], FoundSample) :-
    Sample = sample(SampleWn,SampleRows,_,_),
    (0 is (Wn-SampleWn) mod WindLen, matches(SampleRows, Rows) ->
        FoundSample = Sample
    ;
        find_cycle(WindLen, Wn, Rows, Samples, FoundSample)
    ).

matches([], _).
matches([A|As], [A|Bs]) :- matches(As, Bs).

% Take rows until a section where rocks can't fall through
take_until_unreachable(Rows, Taken) :-
    (unreachable(Rows, N) ->
        take(N, Rows, Taken)
    ;
        Rows = [R|Rows_],
        take_until_unreachable(Rows_, Taken_),
        Taken = [R|Taken_]
    ).

take(0, _, []) :- !.
take(N, [A|As], [A|Taken]) :- N>0, N_ is N-1, take(N_, As, Taken).

unreachable([511|_], 1) :- !.
unreachable([R1,R2|_], 2) :- 511 is R1 \/ R2, !.
unreachable([R1,R2,R3|_], 3) :- 511 is R1 \/ R2 \/ R3.

%----------------------------
% Fall one piece of rock
%----------------------------

fall(Piece, Wind, Wn0, Wn1, Rows0, Rows1) :-
    prepend_empty_rows(Piece, Rows0, Rows0_),
    fall_(Piece, Wind, Wn0, Wn1, Rows0_, Rows1_),
    strip_empty_rows(Rows1_, Rows1).

fall_(Piece, Wind, Nw0, Nw1, Rows0, Rows1) :-
    wind_char(Wind, Nw0, Wc),
    blow(Wc, Rows0, Piece, Piece_),
    Nw0_ is Nw0+1,
    (down_ok(Piece_, Rows0) ->
        %writeln("down"),
        Rows0 = [R0|Rows0_],
        fall_(Piece_, Wind, Nw0_, Nw1, Rows0_, Rows1_),
        Rows1 = [R0|Rows1_]
    ;
        merge(Piece_, Rows0, Rows1),
        Nw1 is Nw0_
    ).

strip_empty_rows([], []).
strip_empty_rows([R0|R0s], R1s) :- walls(R0) -> strip_empty_rows(R0s, R1s) ; R1s = [R0|R0s].

prepend_empty_rows(Piece, Rows0, Rows1) :-
    length(Piece, H),
    NumEmpty is H+3,
    walls(W),
    prepend_n(NumEmpty, W, Rows0, Rows1).

prepend_n(0, _, Rs, Rs) :- !.
prepend_n(N, V, Rs0, Rs1) :- N>0, N_ is N-1, prepend_n(N_, V, [V|Rs0], Rs1).

%----------------------------
% Wind
%----------------------------
wind(Wind) :- input(String), string_wind_array(String, Wind).
wind_test(Wind) :- input_test(String), string_wind_array(String, Wind).

string_wind_array(String, Wind) :-
    string_chars(String, Chars),
    length(Chars, Size),
    functor(Wind, windarr1, Size), % arg hack to get an array
    numlist(1,Size,Idxs),
    maplist(term_setarg(Wind), Idxs, Chars).

term_setarg(Term, Index, Value) :- setarg(Index, Term, Value).

wind_array_length(Wind, Length) :- functor(Wind,_,Length).

wind_char(Wind, Nw0, Char) :-
    functor(Wind,_,Size),
    I is Nw0 mod Size + 1,
    arg(I, Wind, Char).

%----------------------------
% Merge piece with rows
%----------------------------
merge(Piece, Rows0, Rows1) :- mapappend(or, Piece, Rows0, Rows1). 

or(A,B,C) :- C is A \/ B.

mapappend(_, [], L, L) :- !.
mapappend(_, L, [], L) :- !.
mapappend(Goal, [A|As], [B|Bs], [C|Cs]) :- call(Goal, A,B,C), mapappend(Goal, As, Bs, Cs).

%----------------------------
% Hit check
%----------------------------
hit(Piece, Rs) :- any(and_nonzero, Piece, Rs).

and_nonzero(A,B) :- \+ 0 is A /\ B.

any(Goal, [A|As]) :- call(Goal, A) -> true; any(Goal, As).
any(Goal, [A|As], [B|Bs]) :- call(Goal, A, B) -> true; any(Goal, As, Bs).

%----------------------------
% Moving
%----------------------------
shift_left(A, B) :- B is A << 1.
shift_right(A, B) :- B is A >> 1.

try_left(Piece0, Piece1, Rows) :-
    maplist(shift_left, Piece0, Piece1),
    \+ hit(Piece1, Rows).

try_right(Piece0, Piece1, Rows) :-
    maplist(shift_right, Piece0, Piece1),
    \+ hit(Piece1, Rows).

down_ok(Piece, [_|Rows]) :-
    \+ hit(Piece, Rows).

blow('<', Rows, Piece0, Piece1) :- (try_left(Piece0, Piece1, Rows) ->
                                        true%, writeln("left")%, maplist(writeb,Piece0), writeln("-->"), maplist(writeb,Piece1)
                                    ;
                                        Piece1 = Piece0).
blow('>', Rows, Piece0, Piece1) :- (try_right(Piece0, Piece1, Rows) ->
                                        true%, writeln("right")
                                    ;
                                        Piece1 = Piece0). 

%----------------------------
% Input
%----------------------------
input_test(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>").

input(">><>><<><>><>>><<><<<>><<<><>>>><<<>>><<>><<<<>><<<>>>><<<>>><<<<>><<<>>><<<<>><<>>>><>>>><<><<>>>><<>><>>>><<>>>><<<<>>><>><>><<<<><>>><<>>><<><<<><<<<>><><<>>><<<>><<<<>>>><><<<>>>><>>>><<<><<>>><<>><<<>>>><>><>>>><><<<<>><><<<<>><<<<>><><>><<>>>><>>><<<>><<>><<>>><<<>><<<><>>><<<<>>>><<<<>>>><<<>>><>>><<<<>>>><<<<><<>>>><<<>>><<<><><>>>><<<<>>><<>>><<<>>><>>><<<>>>><><<<<>>>><>><<<>><<><<<<>><<<>><>>><<<<>>>><<<<>>>><<<>>><<<>>>><>>><<<><<<>>><<>>>><<<>>><<<>>>><<<<>><<<<>>><<<<><>><<<<>>><<<<>>>><<>>><>><<<<><<<<>>>><<<>>>><<><<<<><<<>><<<<>><>>><<><<<>>><<>><<<<>><<<>>><>>>><<>>><<<>>>><<>>>><<<><<<<>>>><<>>>><>><<<<>><<<>>>><<<<><>><><<>>><<<<>>><<<><><<<<><<<<>><<><>>><>>>><<<>>><<<>><>>>><<<<>><<<>><<>>><<<<>><<<>><<<><>><<>>><>>><<<><<<<>>><<<<>>>><<<><<<><<><<<><<<><<<<><<<>><><>>><<<><<>>><<<>><>><<<><>>>><<<<>>>><<<>>>><<>><><<<<>>><<<<>>><<<<>>>><><<>>>><<<<>>><<<>><<>>>><<<><>><<>>><<<>>>><<<>>>><<<<>><>><<<><<<>>><<>>><<>>>><<<<>><<<>>><>>><<<<>><<<<>>>><<<>>>><<<<><<<<>>><<<><<<<>>><>><<<<>>>><<><>><<<>>><<<>><<>>>><<>>><<>>><<<<>>>><>>>><<<>><>>>><<<<>>>><><<<<>>>><<<<>>>><<<<><<>><<<><<><<<>><<>><<<<><<<>>>><<<<>>>><<><>>>><<<>><<>><<<>>><<<<>>><<<>><<<<><<<<><>>><<<<>><<>>>><<<>>>><<<<>>>><<>>>><<<>>>><>><>>><<>>><<<<>>>><<<>>>><<>>>><<>>>><<<<>>>><>><>><<>><<<<><><<<>><<<<><<<>><<><<>><<<<><<>>>><><<<>>><<<><<<<>>>><>>>><>>>><<<><<<><>>>><><<<>>><><<>>><>>><<<>><<<<><<>>>><<>>><<<<><>><<<<>>><<>><<<<>><<<<>>><<>>>><<>><<>>>><<<>>>><<>>><>>><<<>>><<<<>>><>>><<<<>><<>><<<<>><>><<<<><>><>>>><<<>><<>>><>>><<<>>>><<<<>><<<<>><<>>><<>>>><<<<>>>><<<>><<>><>>>><<<><>><<<>>>><<<>>>><<<>><<><><<<>><<<<><>><>>>><<>><<<><<<<><<<<>>><<<><<>>><<>><<<<>>><<<>><<>>><>>>><<>><<<<><<>><<<<>>><<<>>><<<>><<>>><<<<><<>><<><<<<>><>><>><<>>><>>><<<<><<<>>><<<>>>><<<<>>>><<<<>>><<><<<>>>><<<<>>>><><>>><<<<>>><<<<>>>><<<>>>><<<><<>>>><<<<>>>><>>>><<<><<>>><<<>><>>><<<<><<>>>><<>>>><<<><<<<>>><<<<><<<><<<>>><<<><<<<>><>><<<><<>>><<<>><<<<><<<<><<<<><>>>><<>><>><<<>>>><<<>>><>>>><<<<>><<<><>>><<<<>>><>>>><>>>><<<<>>><<<<>>><<<>><<<<>><<>>><<<<>><<<>>>><>>><<<<>>><><<<<>>><>><<<>>><<<>>>><<<<><<<><<>>>><<<>>><>>>><>>><<>><<<><<>>>><<<<>><<<<><><<>>>><<><<>>>><<>>>><<<<>><<<<><><<<><>><<<<>>><<<<>><<<<>><>><<<<>>>><<<<><>><<<>>>><>>><<<>><<><<<>><<>>><<<><>>><<<<>>><><<<<><<<<>>><<<>>>><<<>>><>>><<<<><<<>>><>><<>><><<<>><<>>><<<<>><<<>>>><<>><>><>><<<<>>><<<><<>>>><<<<>><<>>>><<<>>><<<>><<><>>><<<<><<<>><<<<>>>><<>>><<<>>><>>><<<>>>><<>><<>>><<>><>>><<<>>><<>><<>>><<<>>><<<>><>><<<>>><>><<<>>>><<>><<>><<>>><<>>><<<<>>><<<><<>>>><<>>><<>>>><<<<><<<<>>><<<<>><>>>><<<>>><<<<>><<<<>>>><>>>><>><<<><>>><<>>><<<<>>>><<<<>><<><<><>>><<<<><<<>>><>>><><<>>><<><<><>><<<><>>>><>>>><<>>><<>><<<<>>>><<<>><<<<>>><<<<><<>>><<<<>>>><<<<>>>><><>>><<>><<>>><>>>><<<>>><<<<><>><>>><<<<>>><<<>><<>>>><>>>><<<><<<<><<<<>><<>><<<<>>>><>>>><>>>><<<><<>>><>><>>><<><>><<>>>><<>>><<<>>>><<<<>>>><>><><<>>><<<><<<<>>><>>><>>><<<>>><<<<>><<>><<<<>>><><<<>>>><<<<>><>>><<<<>>><>><>><>>>><<>>><<<<>>>><<<<>>>><<<<>>><<<<>>><<<>><<<<>>><<><<<>>><<<<>>><<<<>>><<>>>><<<>><>><>>><<<<>>><><<<<><<<><<<<>><>>>><<<<>><<<<>>><<<<>>>><<>>><<<<>><>><<>><<<>>>><<<>>><<<>>><<<<>>>><<>>>><<>>>><<<><<<<><<><<<<>><><>><>>><>>>><<<><>>>><<><>><><<<>><<<<>>>><><<><<<>>><<<<><<>>><<<>><<<<><>><>>>><>>><<<><<>><><<<>><<>>><<><<<<>>><<>><<<><<<><><<<<>>><<<<><<<><<<<>>><>>><>>>><<<>>><>>><><<>>>><<<>>><<<>>><>>>><<<>>>><>>><><<>>>><<<<>><<<>><>>><<>>>><<>>>><><><<<<>>>><<<><<>>>><<>><<<><<><>>><>>><<>>>><<><<<>><<><<<<><<<<>><<<<>>><<<>>>><<<<>>>><<<>>>><>>><>>><<>>>><<<<>>><>>><<>>><<<>><<>>><<<><<<<>>>><<<<>>>><<><<<><<><<><>><<<>>>><<<>>><<<<>><<<>><<>><>>>><<<<>>>><<<<>>>><<<>>><>>><><<>>><<<<>><<>>><<<<><>>><>>>><>><>><<<<>>>><>>><<<><<>>><>>><<><>><<>>><<<>><<<<>>>><>>><>>>><>>>><<<<><<>><<<>>><>>><>>>><><><<<<>><>><>>><<<>>><<<<>><<<><><<>>>><>>>><>>>><<<<>>>><<>>>><<<<>>>><>><<<>>><<>>>><<<><<<<>><>>>><<<>>><>>>><<<<><<>><>><>>>><<<<>>><<<<>>>><><>><<<>><><<<><<>><<<<>>>><<<>>>><<<>><><<<<><<<<>>>><<>>><<<<>><<<>><><<<<><<><<<>>><>><<<<>><>>>><<<<><<>>><<<>>>><<<<>>>><<<<><>><<<>><>>>><<<<>>>><<>>><<<<>>>><>><>>>><>>><<<>>>><<<>>><>>>><<<<>>><<<>>><<>><<<<>><<><>><<<<>>>><<>>>><<>><<>>><<<<>><<>>><<<<>>><><>>><<<>><<<>>><<>>><>>>><>>><<>>>><<<<><>>>><<<<><<<>>>><>>><<<>>><<<<>>>><<<<>>><>>><<<><>>>><<<>>><>>><<><<>>>><<><<<<>><<<><<<<>><>><<<<>><>>>><<>>><<<>><<<>>>><>>>><<<<>>>><><<<<>>>><<<<>><<<>>>><<<>><<<<>>>><<>>><>>><<<<>>><>>><><<<<>>>><<<>>><<<<>>>><>>>><<<<>><<<<>><<<>>><<<<>>><<<<>>>><<>><>>>><<<>><<<><>>>><<<<>><<>><<<<>><>>><>>>><<<><<<<>><>><<<<>><<<><<<><><<>>><>>><<<<>><><<<><<<<>>>><>><<<<>>>><<<<>>><<>><<>>>><<<<><<<><<<<>><<<<>>><><<><<<><>>><<<>>>><<<>>><<<<>><<>><<<<>>>><>><><<<><><>><<<>><<<<>>>><<<>>>><<<<>>>><<>>>><>><<<>>><<<>>><<>>><<<<>>><><<>>>><<<>>><>>>><<<>><<<<>><<<>>><<><<><<>><<<><<<>><<<>>><<>><<<<>><<><<<<><<<<>>>><>>>><>><<>>><<<<>>><<>><><>><<<>><<>>><<<<>><><>>>><<<>>>><<><<<><<><<<<>>><<<>><<<>><<<>>>><<>>>><<<<>><<<<>><<<<><<><<>>><<<>>><<>>><<<>>><<<<><<<<>>><<<><<<>>><<<>><<<>><<<<>><<<>><>>>><>><<<<>>>><<<<>>><><<<>>><<>>><><>><<>>><<<><<<>>><<<>><>>><<<>>>><<<>><<<<><<<>>><<<<><<<>>>><><><<<>><><<<><<<<>>><<><<<<><<<>>><<<<><<<<>>><<>>><<>>>><<>>>><<>>><><<<><<<<>><<<<>><<<>><<<<>>><<>>><<<<>><<<<>>>><<>>><<<<>>>><<><<>><<<>><<<<>><<<<>>>><<>>>><<<>>>><<<<>>><>><>>><<<<>>><<>>><<<>>>><<<<>>><<<<>><<>>>><<>>><<<>>>><<<>><<<><<<>>>><<<<>>>><<<>><>><<>>><<<<>><>>>><<><<<>>>><<>>>><<<<>>><<<>><<<<>>><<<<>>>><<<<><<>>>><>><>>><<<>>>><>>>><>>>><<<><<>><<><<>><<<<><<>>><><<>>>><<<<><<<<><<<>><<<>><>>><<><<<><>>>><>><<<>>><<>>><<<<>>>><<<>><<<<>>><<>><<<>><<<<>><>>>><>><<<>>><<<<>>>><<>><<<><<>><<<>><<<<><<<>><<>><<<>>>><<<<>>><<<>><<<><<>>>><<>>>><<<<>><>>>><<<>>><<<><>><<<>>>><>>>><<<<>>>><<<>><>>>><<<<>><<<<>><<<<>><<>><><<<>><<<>>><<><<>><<<<>>>><<>>><><>><>><<>>>><<<<><<>>><<<<>>>><<<>>><<>><<><>><<<<>>><<>><<>>><>>>><<<<>><>><<<><<>>>><><<<><><<>>>><><<>>><<<>>><>>>><<><<>><><><<<<>><<<<><<>>>><>>><<>>><<>>><<<>>><><<>>>><<>>><<<>>>><<<>>><>><<<<><>>>><<<>>><<<>>><<><<>><<<<>>><<<<>><<>>>><>>>><<>><<<<><<<<>><<<<><<<>>>><<><<<<>>>><<<<>>>><>>>><<>><<><<<>>>><<<><<>><<<<>>><<<><>>><>><<<>><><>><<<<>><<<<>>>><<<<><<>>>><>>>><<>>><<<<>><>>><<<>>><<><<>>><>><<>>><><<>>>><>><<<>>>><<>><<>>><<<>>>><><<><<><<>><<><<<<>>>><<<>>><>><<<<>>>><<<<>>>><>>><<<>><<>>>><<>><>>>><>>><<>>>><<<><>><<<>><<<<>><<>><<<>>><<<>>><<<<>><>>>><<<><<<><<>><<<<><<<>><><<><<<<><<<>>>><<<><>><<<><<<>><>>>><><<<<>>><<>>>><<>>><>>>><<>>><<<>>><>><<<<>>><><<><<<<>><>>>><<>>>><>>>><<><>><<<<>><<<<>>><><<<>><<><<<><<<>>><<><<<><<><><>>><<<<>><<><>>>><<<>>>><<<<>><<<<>>><>>>><<>>><<<<>><>>>><<<<>><>>><<><>>><<<><>>>><<<<>>><>><<<<>>>><<<>>>><<>>><>>><><<><<<<>><<<><>>>><>>>><<<>>><>>>><<<><<<>><><>><<<><>>><>><>><>>><<<<>>><<<<>><<<<>>>><<<>><<<>>>><<>>>><<>>>><><<<>>>><<>><<<>><<>>>><<<>>><<>>><<<><<<>><>>><>><<<<>><<<>><>>><<<<>><>>>><>><<><<<<>><>><<<<><<>>><<<><>>><<<><<><<<>><<<<>>><<<>>>><<<<>><<>>><>><><<>>><>><<<>><<>>><<>>>><<>>><<>>>><>>><>>><<>><<>>>><<<><<<<>>><<<>>><>>>><<<>>><<>><><<>>>><<<<>>><<><<>>>><>><>>><<<<><<<<>><<>><<<<>><>>>><>><<<>>>><<>><<<>>><<<>>>><<<>><<<><<<<>><<>>>><<<>>>><<<><>>><<>><<<<>>>><<>><<>>><><<<<><<<<><<>>>><<<>>>><<<<><<>>>><<<<>><>><<<>><<>><<<<>>><<>>><<<>><>>><<><<<>>><>>><><<<><<<<>>>><<<<>>>><<<<>>><<>>><<<>>>><<<>><<<<>>><<<<>>>><<<>>><<<>>><<<<><>>><>><<>>>><>>>><<<<>><<>><>>>><<>>>><<<<>>><<<><<<<>>>><<<>>>><>>>><<>>>><<<<>>>><<><<>>>><<<><<<>><<<>><<>>>><<<>>>><<<>><<<<>>>><>>>><<<><>>>><<>>><<<<>><><<<><>>><<>>><<<<>>><<<<>>><>>>><<>>>><<>><<<<><<<>>><<>>>><<<><<<>>><<>><<<>><<>><<><>><<<><<><<<>>>><><<><>>>><<<>>><><<<>>>><<<>><<<<><<<><>>><>>>><<<><<>><>>><<>><<<<>>><<<><<<>>>><<><<>>><<>><>>>><<<<>>><>>><<><<<>>>><><<<>>><><>>><>>><<<<><<<<><<<<>>>><>>>><<<<>><<<><<>>>><>>>><>>><><<<>>><<<<>><<>><<>>>><<>><><>><>><<<<>><<<>>>><<<><<<<><<<<><<<>>>><<><<<<>>>><<<>><>>>><>><<<>>><<<>>><<<<><<<<><<<>><<<<>><>><<<>><<<<>>><<<<><><<<><<>>>><<><<<<>>>><<<><>>>><<><<<<><>>><<<>>><<<<>>><<>><>><<<<>>><<>><<<<>><<<>><>><<<<><>>>><<<>><>>>><<<<><<<>><<>><<>><<>>><<<><<>>><<<>>><<<<>>>><><>>><<<<>>><>>><<<>><>><<<<>>><<>>><<><<><><<<><<>><<<>>>><<>>><>>>><>>><<>>>><<<<>><<>>><<<><<<>><<<<>><<><>><><<><>><<>>>><<<>>>><<<<>><<<<>><<<>><<<<><<>>><<<<>>>><<<>><<<<>>>><>>>><<<<><>><>><<<<>>><<>>>><<<<>><>><>>>><<<<><<>>>><<>><<>>>><<<<>>><<<<><>><>>><<<<>>>><<<>>><<><<><<<<>><<<>>>><>><<>><><<<<>>><<<>>>><<<<>>>><>>><<<<>>><<>><>>>><<>>><>>>><<<<><<>>>><<>>><<>><<<<>>>><<><<<><><<<>>><>>>><<<>>>><<<>>>><<><<>>>><<<><<>>><<><<><<>>>><<>>><<>>><<<<>><<<>>>><<<>>>><<<><<<>>><<<><<<>><<<<><<<><<>>>><>>>><<<<>>>><<>>>><<<<><<<>>>><<<>>>><><>>><<>>>><<<>>><<<<>>><>>>><<>><>><><<<>><>><<><<<<>>>><<<>><<<<>><><>>>><><<<<><><<<<>>><<<>>><<<<>>><<<<>>><>>><<<>>><<<<>>><<<>>><<>>><<>>>><<<>>><<<<>>>><<<<><><<<>>><<<><<<<>><<<<>>><<<>>>><<<<><<<>>><<>>><<<<><>>><<><<<<>>>><<<<>>><>>><<<<>><<<<><><>><>><<<<>>><<<<>><<<<>>><>>>><<<<>>><<<>>><<>>><<><<>>>><>><>>>><>><>><>>><<><<><><<<>>><<><<>>>><<<>>><<<><>>><<<>>>><<<<>>><>>>><>>><<<><<><>>>><<><<<>><<>><<<>><<<<>><<>>>><<<<><<<><>>>><<><<>><<<<>><<>>><<>><<<<>>>><<<>>>><<<>>><<<>>>><<>><<<<><<<><<><<<>><<<<>>><<<>><>>>><<<<><<<<>>><>>>><<<>><<>>>><>>><<<>><<<<>>>><<>><<><>>><<<<><><<<>><<<>><<<<>>>><<<><>>>><<><<<><<<<>>><><><<<>><<<>><<<<>>><<<>>><<<<><<<>>>><>><<><<>>>><<<>>>><<>>>><<<<>>><<>>>><<<>><<<><<<>>><<>>><<<<><<<>>>><<<>>><<<<>>><>><<<>>><<>>><<<>>><<>><<<<>><>>><<<>><>><>>><<<><>>>><<<<>>><<<<><>>><>>><<<>>><<<>>><<>>><<<>><<<>><<<<><><><><<<>>>><<<<>>><<><>>><>>>><<>>><<<<>>><<<><>><<<>>><>>>><>>><<>><<<<>>><<<><><<<<>>><<<>>>><<>>><<<<><><<>>><<<<>><<>>>><<<>>><<<<>>><<>>><<<>>><<<<>>><<<<>>><<><<><<<>>><<<>>>><><<>>>><<<>><<>>><<<>>>><<<><<>><>>>><<<>><<>><<>>>><<<><><<<>><<>><<><<<><>>><<><<<<>>>><>>><>>><<><<<>><<><<<>>>><<<>>><><<<<>><<<<><<<<>>><<<<>><<>><<<<><<<<>>><<<<>>>><<>>><<<>>><<>>><<<>>><<<>><<>>><>>><<<>><<>><><>><>>><<>><<><>>>><>>>><<>>>><<>>><<<>>><<<<>>>><<<<>>>><<<<>>><<><>>>><>><<>>>><<<<>>>><<>>><<<<>><<<>>><<<<>><>>>><<<<>><<<><<><<<<>").
