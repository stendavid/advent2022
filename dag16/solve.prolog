
solve1(File, MaxRelease, Ops) :-
    read_input(File,Vs),
    assert_dist_table_with_working_valves("AA", Vs),
    working_valves(Vs,Vok),
    set_bound(0),
    findall(TotalRelease-Ops, open_v1("AA", 30, Vok, 0, TotalRelease, Ops), Rs),
    max_member(MaxRelease-Ops_, Rs),
    reverse_ops_time(31, Ops_, Ops).

solve2(File, MaxRelease, Ops) :-
    read_input(File,Vs),
    assert_dist_table_with_working_valves("AA", Vs),
    working_valves(Vs,Vok),
    set_bound(0),
    sum_valve_rate(Vok, RateSum),
    findall(TotalRelease-Ops, open_v2(("AA","AA"), (26,26), Vok, 0, RateSum, TotalRelease, Ops), Rs),
    max_member(M, Rs),
    M = MaxRelease-Ops_,
    reverse_ops_time(27, Ops_, Ops).

reverse_ops_time(TMax, open(V,T,P), open(V,T_,P)) :- T_ is TMax-T, !.
reverse_ops_time(TMax, open(V,T), open(V,T_)) :- T_ is TMax-T, !.

reverse_ops_time(TMax, Ops, Ops_) :- is_list(Ops), maplist(reverse_ops_time(TMax), Ops, Ops_).

%----------------------------------
% Open valves
%----------------------------------
open_next(CurName, Tleft, Vs0, Vs1, OpenedName, OpenedRate, Release, Tspent) :-
    sort_by_release(CurName, Tleft, Vs0, VsSort, _),
    select(OpenedValve, VsSort, Vs1),
    valve_release(Tleft, CurName, OpenedValve, Release, Tspent),
    valve_name(OpenedValve, OpenedName),
    valve_rate(OpenedValve, OpenedRate).

open_v1(_, _, [], R, R, []) :- update_bound(R).
open_v1(_, 0, _, R, R, []) :- update_bound(R).
open_v1(Cur, Tleft, ValvesLeft, SumRelease, TotalRelease, Ops) :-
    open_next(Cur, Tleft, ValvesLeft, ValvesLeft_, OpenedName, _, AddedRelease, Tspent),
    SumRelease_ is SumRelease + AddedRelease,
    Tleft_ is max(0, Tleft - Tspent),
    % Branch-and-bound: fail if we can't possibly get a release above the bound
    bound(B),
    sum_valve_rate(ValvesLeft_, RateLeft),
    MaxRelease is SumRelease_ + RateLeft * max(0,Tleft_-1),
    MaxRelease > B,
    % Bound can still be reached, open next valve
    open_v1(OpenedName, Tleft_, ValvesLeft_, SumRelease_, TotalRelease, Ops0),
    Ops = [open(OpenedName, Tleft_)|Ops0].

open_v2(_, _, [], R, _, R, []) :- update_bound(R).
open_v2(_, 0, _, R, _, R, []) :- update_bound(R).
open_v2(Cur, Tleft, ValvesLeft, SumRelease, RateLeft, TotalRelease, Ops) :-
    Cur = (C1, C2),
    Tleft = (Tl1, Tl2),
    % you
    (   Tl1 >= Tl2
    ->  open_next(C1, Tl1, ValvesLeft, ValvesLeft1, OpenedName1, OpenedRate1, AddedRelease1, Tspent1),
        Tl1_ is max(0, Tl1 - Tspent1),
        Ops1 = [open(OpenedName1, Tl1_, you)]
    ;   Tl1_ = Tl1,
        Ops1 = [],
        ValvesLeft1 = ValvesLeft,
        AddedRelease1 = 0,
        OpenedName1 = C1,
        OpenedRate1 = 0,
        RateLeft1_ = RateLeft
    ),
    % elephant
    (   Tl2 >= Tl1, ValvesLeft1 = [_|_] 
    ->  open_next(C2, Tl2, ValvesLeft1, ValvesLeft_, OpenedName2, OpenedRate2, AddedRelease2, Tspent2),
        Tl2_ is max(0, Tl2 - Tspent2),
        Ops2 = [open(OpenedName2, Tl2_, elephant)]
    ;   Tl2_ = Tl2,
        Ops2 = [],
        ValvesLeft_ = ValvesLeft1,
        AddedRelease2 = 0,
        OpenedName2 = C2,
        OpenedRate2 = 0,
        RateLeft_ = RateLeft1_
    ),
    SumRelease_ is SumRelease + AddedRelease1 + AddedRelease2,
    RateLeft_ is RateLeft - OpenedRate1 - OpenedRate2,
    % Branch-and-bound: fail if we can't possibly get a release above the bound
    bound(B),
    MaxRelease is SumRelease_ + RateLeft_ * max(0, (max(Tl1_,Tl2_)-1)),
    MaxRelease > B,
    % Bound can still be reached, open next valve
    open_v2((OpenedName1,OpenedName2), (Tl1_,Tl2_), ValvesLeft_, SumRelease_, RateLeft_, TotalRelease, Ops0),
    append([Ops1, Ops2, Ops0], Ops).

%----------------------------------
% Bound for branch-and-bound
%----------------------------------
:- dynamic bound/1.
bound(0).

set_bound(B) :-
    retractall(bound(_)),
    assertz(bound(B)),
    write("New bound: "), writeln(B).

update_bound(Bnew) :-
    bound(Bcur),
    (   Bnew > Bcur
    ->  set_bound(Bnew)
    ;   true).

%----------------------------------
% Total released pressure
%----------------------------------
% Total pressure released if going from Nfrom to Vto and opening it, when Tleft minutes remain
valve_release(Tleft, Nfrom, Vto, Rsum, Tspent) :-
    Vto = v(N,R,_),
    dist(Nfrom, N, D),
    Tspent is D+1,
    Topen is max(0,Tleft-Tspent),
    Rsum is R*Topen.

valve_release(Tleft, Nfrom, Vto, Rsum) :- valve_release(Tleft, Nfrom, Vto, Rsum, _).

% Sort Vs by total pressure released (decr) if going there from Ncur and opening the valve, when Tleft minutes remain
sort_by_release(Ncur, Tleft, Vs, VsSorted, PairsRev) :-
    map_list_to_pairs(valve_release(Tleft, Ncur), Vs, RsumVpairs),
    keysort(RsumVpairs, PairsSorted),
    reverse(PairsSorted, PairsRev),
    pairs_values(PairsRev, VsSorted).

%----------------------------------
% Distance table
%----------------------------------
% abolish is needed in SWI to keep 'dist_' tabled after reload
:- abolish(dist_/3).
:- table dist_(_,_,min).
:- dynamic edge/2.
:- dynamic dist/3.

% The tabled 'dist_/3' is slow compared to "normal" predicates. Therefore we
% only use it to create 'dist/3' which is used for searching.

dist_(X, Y, L) :- dist_(Z, Y, L0), edge(X, Z), L is L0+1.
dist_(X, X, 0).

assert_edges(Es) :-
    abolish_all_tables, % needed since we change 'edge', maybe incremental on 'dist' could be used instead 
    retractall(edge(_,_)),
    maplist(assertz,Es).

assert_dist_table_with_working_valves(InitialValve, Valves) :-
    % rebuild edge/2
    valves_edges(Valves, Edges),
    assert_edges(Edges),
    % use dist_/3 to create dist(A,B,Dist) for all pairs of working valves A,B
    retractall(dist(_,_,_)),
    working_valves(Valves, Working),
    maplist(valve_name, Working, WorkingNames_),
    assure_member(InitialValve, WorkingNames_, WorkingNames),
    findall(A-B, (member(A, WorkingNames), member(B, WorkingNames)), ValvePairs),
    maplist(valve_pair_dist, ValvePairs, Ds),
    maplist(assertz, Ds).

valve_pair_dist(A-B, dist(A,B,Dist)) :- dist_(A,B,Dist).

assure_member(M, L0, L1) :-
    (   memberchk(M, L0)
    ->  L1 = L0
    ;   append([M], L0, L1)).

%----------------------------------
% Valves and edges (tunnels)
%----------------------------------
vt_edge(V, To, edge(V,To)).

valves_edges([], []).
valves_edges([V|Vs], Es) :-
    V = v(Name,_,Nbrs),
    maplist(vt_edge(Name), Nbrs, Es0),
    valves_edges(Vs, Es1),
    append(Es0, Es1, Es).

valve_name(v(N,_,_), N) :- !.
valve_name(v(_,_,_), _) :- !.
valve_name(A, _) :- type_error(v(...), A).

valve_rate(v(_,R,_), R) :- !.
valve_rate(v(_,_,_), _) :- !.
valve_rate(A, _) :- type_error(v(...), A).

rate_valve(R, v(_,R,_)).

working_valves(Vs, Vok) :- exclude(rate_valve(0), Vs, Vok).

sum_valve_rate(Vs, Sum) :- maplist(valve_rate, Vs, Rs), sum_list(Rs, Sum).

%----------------------------------
% Read input
%----------------------------------
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

name(Name) --> string_without(" ,\n", Codes), { string_codes(Name, Codes) }.
names(Names) --> sequence(name, ", ", Names).

valve(v(Name, Rate, Nbrs)) -->
    "Valve ", name(Name), " has flow rate=", integer(Rate), "; ",
    ( "tunnels lead to valves " | "tunnel leads to valve "), names(Nbrs), eol.

read_input(File, Valves) :- phrase_from_file(sequence(valve, Valves), File).

%-----------------------------------------
% DCG helpers
%-----------------------------------------
sequence(G, [R]) --> call(G, R).
sequence(G, [R|Rs]) --> call(G, R), sequence(G, Rs).

sequence(G, _, [R]) --> call(G, R).
sequence(G, Sep, [R|Rs]) --> call(G, R), Sep, sequence(G, Sep, Rs).