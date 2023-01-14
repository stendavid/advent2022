while_not_eof(Stream) :- \+ at_end_of_stream(Stream), (true; while_not_eof(Stream)).

read_line(Input, []) :- peek_char(Input, '\n'), get_char(Input, _), !.
read_line(Input, [C|Cs]) :- get_char(Input, C), read_line(Input, Cs).


%    Lowercase item types a through z have priorities 1 through 26. Char code: 97-122
%    Uppercase item types A through Z have priorities 27 through 52. Char code: 65-90
priority(Char, P) :-
    char_code(Char, C),
    (C >= 97, C =< 122 ->
        P is C - 97 + 1
    ;
        P is C - 65 + 27).


split(List, [], Second, 0) :- Second = List, !.
split([L|Ls], [F|Fs], Second, N) :- F = L, N2 is N-1, split(Ls, Fs, Second, N2).

split(List, First, Second) :-
    length(List, Len),
    Len2 is Len//2,
    split(List, First, Second, Len2).

read_input(Line) :-
    open('input',read,Input),
    while_not_eof(Input),
    read_line(Input, Line).

read_input3(Line1, Line2, Line3) :-
    open('input',read,Input),
    while_not_eof(Input),
    read_line(Input, Line1),
    read_line(Input, Line2),
    read_line(Input, Line3).

find_common_char(L1, L2, CommonChar) :-
    sort(L1, L1Unique),  % sort() removes duplicates
    member(CommonChar, L1Unique),
    memberchk(CommonChar, L2).

find_common_char(L1, L2, L3, CommonChar) :-
    sort(L1, L1Unique),  % sort() removes duplicates
    member(CommonChar, L1Unique),
    memberchk(CommonChar, L2),
    memberchk(CommonChar, L3).

read_priorities(P) :- 
    read_input(Line),
    split(Line, First, Second),
    find_common_char(First, Second, CommonChar),
    priority(CommonChar, P).

read_badge_priority(P) :- 
    read_input3(L1, L2, L3),
    find_common_char(L1, L2, L3, CommonChar),
    priority(CommonChar, P).

solve(Sum) :- findall(C, read_priorities(C), Cs), sum_list(Cs, Sum).
solve2(Sum) :- findall(C, read_badge_priority(C), Cs), sum_list(Cs, Sum).
