% copied from predsort, and changed to not remove duplicates
mysort(P, L, R) :-
    '$skip_list'(N, L, Tail),
    (   Tail == []
    ->  mysort(P, N, L, _, R1),
        R = R1
    ;   must_be(L, list)
    ).

mysort(P, 2, [X1, X2|L], L, R) :-
    !,
    call(P, Delta, X1, X2),
    sort2(Delta, X1, X2, R).
mysort(_, 1, [X|L], L, [X]) :- !.
mysort(_, 0, L, L, []) :- !.
mysort(P, N, L1, L3, R) :-
    N1 is N // 2,
    plus(N1, N2, N),
    mysort(P, N1, L1, L2, R1),
    mysort(P, N2, L2, L3, R2),
    predmerge(P, R1, R2, R).

sort2(<, X1, X2, [X1, X2]).
%sort2(=, X1, _,  [X1]).
sort2(=, X1, X2,  [X1, X2]).
sort2(>, X1, X2, [X2, X1]).

predmerge(_, [], R, R) :- !.
predmerge(_, R, [], R) :- !.
predmerge(P, [H1|T1], [H2|T2], Result) :-
    call(P, Delta, H1, H2),
    !,
    predmerge(Delta, P, H1, H2, T1, T2, Result).

predmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
    predmerge(P, [H1|T1], T2, R).
%predmerge(=, P, H1, _, T1, T2, [H1|R]) :-
%    predmerge(P, T1, T2, R).
predmerge(=, P, H1, H2, T1, T2, Result) :-
    predmerge(<, P, H1, H2, T1, T2, Result).
predmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
    predmerge(P, T1, [H2|T2], R).