read_lines(File, Lines) :-
    open(File, read, In),
    call_cleanup(
        stream_lines(In, Lines),
        close(In)).

stream_lines(In, Lines) :-
    my_read_string(In, _, Str),
    split(Str, '\n', Lines).

my_read_string(In, 0, S) :-
    get_char(In, C),
    (   C = end_of_file
    ->  S = []
    ;   my_read_string(In, _, S0),
        S = [C|S0]).

split([], _, [[]]).
split([C|Cs], C, [[]|Ls]) :- split(Cs, C, Ls), !.
split([C|Cs], Sep, [[C|Cs0]|Ls]) :- split(Cs, Sep, [Cs0|Ls]), !.
