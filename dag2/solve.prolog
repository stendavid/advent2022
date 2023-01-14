

while_not_eof(Stream) :- \+ at_end_of_stream(Stream), (true; while_not_eof(Stream)).

% X = Rock - 1 poäng
% Y = Papaer - 2 poäng
% Z = Scissors - 3 poäng
score('A', 'X', S) :- S is 3 + 1.      % Rock - Rock           -
score('A', 'Y', S) :- S is 6 + 2.      % Rock - Paper          win
score('A', 'Z', S) :- S is 0 + 3.      % Rock - Scissors       lose
score('B', 'X', S) :- S is 0 + 1.      % Paper - Rock          lose
score('B', 'Y', S) :- S is 3 + 2.      % Paper - Paper         -
score('B', 'Z', S) :- S is 6 + 3.      % Paper - Scissors      win
score('C', 'X', S) :- S is 6 + 1.      % Scissors - Rock       win
score('C', 'Y', S) :- S is 0 + 2.      % Scissors - Paper      lose
score('C', 'Z', S) :- S is 3 + 3.      % Scissors - Scissors   -

% X = Need to lose
% Y = Need to draw
% Z = Need to win
score2('A', 'X', S) :- S is 0 + 3.      % L: Rock - Scissors 3p
score2('A', 'Y', S) :- S is 3 + 1.      % D: Rock - Rock 1p
score2('A', 'Z', S) :- S is 6 + 2.      % W: Rock - Paper 2p
score2('B', 'X', S) :- S is 0 + 1.      % L: Paper - Rock 1p
score2('B', 'Y', S) :- S is 3 + 2.      % D: Paper - Paper 2p
score2('B', 'Z', S) :- S is 6 + 3.      % W: Paper - Scissors 3p
score2('C', 'X', S) :- S is 0 + 2.      % L: Scissors - Paper 2p
score2('C', 'Y', S) :- S is 3 + 3.      % D: Scissors - Scissors 3p
score2('C', 'Z', S) :- S is 6 + 1.      % W: Scissors - Rock 1p

read_line(Input, C1, C2) :-
    get_char(Input, C1),
    get_char(Input, ' '),
    get_char(Input, C2),
    get_char(Input, '\n').

read_input(C1, C2) :-
    open('input',read,Input),
    while_not_eof(Input),
    read_line(Input, C1, C2).

s(Score) :-
    read_input(C1, C2),
    score(C1, C2, Score).

s2(Score) :-
    read_input(C1, C2),
    score2(C1, C2, Score).

solve(Sum) :- findall(C, s(C), Cs), sum_list(Cs, Sum).
solve2(Sum) :- findall(C, s2(C), Cs), sum_list(Cs, Sum).
