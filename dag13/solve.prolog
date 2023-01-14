
:- [mysort].

pair_order_ok(pair(P0,P1)) :- comp_(P0,P1,'<').

ok_index_sum(Pairs, Sum) :- ok_index_sum_(Pairs, 1, 0, Sum).

ok_index_sum_([], _, S, S).
ok_index_sum_([P|Ps], Idx, S0, S1) :- P = pair(_,_),
    (   pair_order_ok(P) -> S0_ is S0 + Idx
    ;                       S0_ is S0),
    Idx1 is Idx + 1,
    ok_index_sum_(Ps, Idx1, S0_, S1).

pair_unpacked(pair(P0,P1), [packet(P0), packet(P1)]).

pairs_packets(Pairs, Packets) :- maplist(pair_unpacked, Pairs, Pack0), flatten(Pack0, Packets).

sort_packets(Packets, Sorted) :- mysort(comp2_, Packets, Sorted).

comp2_(Op, packet(A), packet(B)) :- comp_(A, B, Op).

comp_([], [], '=').
comp_([], [_|_], '<').
comp_([_|_], [], '>').
comp_([A|As], [B|Bs], Op) :-
    comp_(A,B,Op0),
    (   Op0 = '=' -> comp_(As, Bs, Op)
    ;                Op = Op0).
comp_(A, B, Op) :-
    integer(A),
    integer(B),
    (   A = B ->  Op = '='
    ;   A < B ->  Op = '<'
    ;             Op = '>').
comp_(A, B, Op) :- integer(A), is_list(B), comp_([A], B, Op).
comp_(A, B, Op) :- is_list(A), integer(B), comp_(A, [B], Op).

div1( packet( [[2]] ) ).
div2( packet( [[6]] ) ).

append_dividers(In, Out) :- div1(D1), div2(D2), append(In, [D1, D2], Out).

find_dividers(Packets, I1, I2) :-
    div1(D1), nth1(I1, Packets, D1),
    div2(D2), nth1(I2, Packets, D2).

%------------------------------------------
% Solve
%------------------------------------------
solve1(File, Result) :-
    read_input(File, Pairs),
    ok_index_sum(Pairs, Result).

solve2(File, Result) :-
    read_input(File, Pairs),
    pairs_packets(Pairs, Pkts),
    append_dividers(Pkts, Pkts1),
    sort_packets(Pkts1, Sorted),
    find_dividers(Sorted, Idx1, Idx2),
    Result is Idx1 * Idx2.

%------------------------------------------
% Read input
%------------------------------------------
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

line(Term) --> string_without("\n", S), eol, { read_term_from_atom(S,Term,[]) }.

pair(pair(T1,T2)) --> line(T1), line(T2).

pairs([P]) --> pair(P), eos.
pairs([P|Ps]) --> pair(P), eol, pairs(Ps).

read_input(File, Pairs) :- phrase_from_file(pairs(Pairs), File). 
