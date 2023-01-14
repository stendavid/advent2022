
at_block_end(Input) :- peek_char(Input, '\n') ; at_end_of_stream(Input).

read_block(Input, SumIn, SumAcc) :- 
    read_number(Input, Num),
    get_char(Input,_),          %skip '\n' char
    (at_block_end(Input) ->
        SumAcc is (Num + SumIn)
    ;
        read_block(Input, SumIn + Num, SumAcc)).

read_block(Input, Sum) :- read_block(Input, 0, Sum).

while_not_eof(Stream) :- \+ at_end_of_stream(Stream), (true; while_not_eof(Stream)).

sums(Sum) :-
    open('input',read,Input),
    while_not_eof(Input),
    read_block(Input, Sum).

max3(List, [A,B,C]) :- msort(List, Sorted), suffix([A,B,C], Sorted).

solve(Max) :- findall(C,sums(C),Cs), max_list(Cs, Max).
solve2(Sum) :- findall(C,sums(C),Cs), max3(Cs, [A,B,C]), Sum is A+B+C, !.
