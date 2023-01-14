while_not_eof(Stream) :- \+ at_end_of_stream(Stream), (true; while_not_eof(Stream)).

read_input(Range1, Range2) :-
    open('input',read,Input),
    while_not_eof(Input),
    read_number(Input, Min1), get_char(Input, '-'), read_number(Input, Max1),
    get_char(Input, ','), 
    read_number(Input, Min2), get_char(Input, '-'), read_number(Input, Max2),
    get_char(Input, '\n'),
    Range1 = [Min1, Max1],
    Range2 = [Min2, Max2].

fully_contains([Min1, Max1], [Min2, Max2]) :-
    Min1 >= Min2, Max1 =< Max2, !; % cut is needed to avoid double solutions when
    Min2 >= Min1, Max2 =< Max1.    % the ranges are the same

overlaps([Min1, Max1], [Min2, Max2]) :- Max1 >= Min2, Min1 =< Max2.

read_fully_containing([R1, R2]) :- 
    read_input(R1, R2),
    fully_contains(R1, R2).

read_overlapping([R1, R2]) :- 
    read_input(R1, R2),
    overlaps(R1, R2).

solve(N) :- findall(C, read_fully_containing(C), Cs), length(Cs, N).
solve2(N) :- findall(C, read_overlapping(C), Cs), length(Cs, N).
