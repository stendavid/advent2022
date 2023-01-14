:- abolish(test/2).

:- table test(_,min).% as subsumptive.

test(1,3) :- writeln("test(1,3)").
test(1,2) :- writeln("test(1,2)").

test(2,X) :- member(X,[3,4,5]), format("test(2,~w)~n",[X]).

test(3,X) :- 
    format("test(3,~w) ENTER~n",[X]),
    (
        format("test(3,~w)+a1~n",[X]),
        test(3,X),
        format("test(3,~w)+a2~n",[X])
    ;
        X=2,
        format("test(3,2)+b~n",[])
    ;
        X=1,
        format("test(3,1)+b~n",[])
    ),
    format("test(3,~w) EXIT~n",[X]).

test(4,X) :- test(5,X1), test(1,X2), X is min(X1,X2).
test(5,X) :- test(4,X1), test(2,X2), X is min(X1,X2).