
items_to(_, [], []).
items_to(N, [send(Item,N)|As], [Item|Items]) :- items_to(N,As,Items).
items_to(N, [send(_,M)|As], Items) :- M \= N, items_to(N,As,Items).

sendto(CurrentN, _, M, MOut) :- 
    monkey_number(M, CurrentN),
    monkey_count(M, C0),
    monkey_items(M, Items), length(Items, L),
    C1 is L+C0,
    monkey_with_items_and_count([], C1, M, MOut), !.

sendto(CurrentN, Actions, M, MOut) :-
    monkey_number(M, N), N \= CurrentN,
    items_to(N, Actions, ItemsTo),
    monkey_items(M, Items),
    append(Items, ItemsTo, ItemsOut),
    monkey_with_items(ItemsOut, M, MOut).

send(CurrentN, Actions, Monkeys, MonkeysOut) :- maplist(sendto(CurrentN, Actions), Monkeys, MonkeysOut).

monkey_inspect(M, Actions) :-
    monkey_items(M, Items),
    monkey_function(M, InspectFunc),
    maplist(call(InspectFunc),Items,Actions).

round(MonkeysIn, MonkeysOut) :-
    length(MonkeysIn, L), L0 is L-1,
    numlist(0, L0, Idx),
    round(Idx, MonkeysIn, MonkeysOut).

round([], M, M).
round([MonkeyIdx|MonkeyIdxs], MonkeysIn, MonkeysOut) :-
    nth0(MonkeyIdx, MonkeysIn, M),
    monkey_inspect(M, Actions),
    send(MonkeyIdx, Actions, MonkeysIn, MonkeysOut0),
    round(MonkeyIdxs, MonkeysOut0, MonkeysOut).

play(0, M, M).
play(N, MonkeysIn, MonkeysOut) :- N > 0, N1 is N-1, round(MonkeysIn, MonkeysIn2), play(N1, MonkeysIn2, MonkeysOut).

%--------------------------------------
% solve
%--------------------------------------
monkey_business(Monkeys, V) :-
    maplist(monkey_count, Monkeys, Cs),
    sort(0, @>=, Cs, Sorted),
    Sorted = [S0, S1|_],
    V is S0 * S1.

% part 1
solve_test(MonkeyBusiness) :-
    init_monkeys_test(MonkeysStart),
    play(20, MonkeysStart, Monkeys),
    monkey_business(Monkeys, MonkeyBusiness).

solve(MonkeyBusiness) :-
    init_monkeys(MonkeysStart),
    play(20, MonkeysStart, Monkeys),
    monkey_business(Monkeys, MonkeyBusiness).

% part 2
solve_test2(MonkeyBusiness) :-
    init_monkeys_test2(MonkeysStart),
    play(10000, MonkeysStart, Monkeys),
    monkey_business(Monkeys, MonkeyBusiness).

solve2(MonkeyBusiness) :-
    init_monkeys2(MonkeysStart),
    play(10000, MonkeysStart, Monkeys),
    monkey_business(Monkeys, MonkeyBusiness).

%--------------------------------------
% init monkeys
%--------------------------------------
% test part 1
init_monkeys_test([
    monkey(0, test_inspect0, 0, [79, 98]),
    monkey(1, test_inspect1, 0, [54, 65, 75, 74]),
    monkey(2, test_inspect2, 0, [79, 60, 97]),
    monkey(3, test_inspect3, 0, [74])
]).

test_inspect0(V, send(V1,To)) :- V1 is (V*19) // 3, if_div(V1, 23, 2, 3, To).
test_inspect1(V, send(V1,To)) :- V1 is (V+6)  // 3, if_div(V1, 19, 2, 0, To).
test_inspect2(V, send(V1,To)) :- V1 is (V*V)  // 3, if_div(V1, 13, 1, 3, To).
test_inspect3(V, send(V1,To)) :- V1 is (V+3)  // 3, if_div(V1, 17, 0, 1, To).

% test part 2
init_monkeys_test2([
    monkey(0, test2_inspect0, 0, [79, 98]),
    monkey(1, test2_inspect1, 0, [54, 65, 75, 74]),
    monkey(2, test2_inspect2, 0, [79, 60, 97]),
    monkey(3, test2_inspect3, 0, [74])
]).

test2_inspect0(V, send(V1,To)) :- V1 is (V*19) mod 96577, if_div(V1, 23, 2, 3, To).
test2_inspect1(V, send(V1,To)) :- V1 is (V+6)  mod 96577, if_div(V1, 19, 2, 0, To).
test2_inspect2(V, send(V1,To)) :- V1 is (V*V)  mod 96577, if_div(V1, 13, 1, 3, To).
test2_inspect3(V, send(V1,To)) :- V1 is (V+3)  mod 96577, if_div(V1, 17, 0, 1, To).

% real part 1
init_monkeys([
    monkey(0, inspect0, 0, [57, 58]),
    monkey(1, inspect1, 0, [66, 52, 59, 79, 94, 73]),
    monkey(2, inspect2, 0, [80]),
    monkey(3, inspect3, 0, [82, 81, 68, 66, 71, 83, 75, 97]),
    monkey(4, inspect4, 0, [55, 52, 67, 70, 69, 94, 90]),
    monkey(5, inspect5, 0, [69, 85, 89, 91]),
    monkey(6, inspect6, 0, [75, 53, 73, 52, 75]),
    monkey(7, inspect7, 0, [94, 60, 79])
]).

inspect0(V, send(V1,To)) :- V1 is (V*19) // 3, if_div(V1, 7, 2, 3, To).
inspect1(V, send(V1,To)) :- V1 is (V+1)  // 3, if_div(V1, 19, 4, 6, To).
inspect2(V, send(V1,To)) :- V1 is (V+6)  // 3, if_div(V1, 5, 7, 5, To).
inspect3(V, send(V1,To)) :- V1 is (V+5)  // 3, if_div(V1, 11, 5, 2, To).
inspect4(V, send(V1,To)) :- V1 is (V*V)  // 3, if_div(V1, 17, 0, 3, To).
inspect5(V, send(V1,To)) :- V1 is (V+7)  // 3, if_div(V1, 13, 1, 7, To).
inspect6(V, send(V1,To)) :- V1 is (V*7)  // 3, if_div(V1, 2, 0, 4, To).
inspect7(V, send(V1,To)) :- V1 is (V+2)  // 3, if_div(V1, 3, 1, 6, To).

% real part 2
init_monkeys2([
    monkey(0, inspect0_2, 0, [57, 58]),
    monkey(1, inspect1_2, 0, [66, 52, 59, 79, 94, 73]),
    monkey(2, inspect2_2, 0, [80]),
    monkey(3, inspect3_2, 0, [82, 81, 68, 66, 71, 83, 75, 97]),
    monkey(4, inspect4_2, 0, [55, 52, 67, 70, 69, 94, 90]),
    monkey(5, inspect5_2, 0, [69, 85, 89, 91]),
    monkey(6, inspect6_2, 0, [75, 53, 73, 52, 75]),
    monkey(7, inspect7_2, 0, [94, 60, 79])
]).

inspect0_2(V, send(V1,To)) :- V1 is (V*19) mod 9699690, if_div(V1, 7, 2, 3, To).
inspect1_2(V, send(V1,To)) :- V1 is (V+1)  mod 9699690, if_div(V1, 19, 4, 6, To).
inspect2_2(V, send(V1,To)) :- V1 is (V+6)  mod 9699690, if_div(V1, 5, 7, 5, To).
inspect3_2(V, send(V1,To)) :- V1 is (V+5)  mod 9699690, if_div(V1, 11, 5, 2, To).
inspect4_2(V, send(V1,To)) :- V1 is (V*V)  mod 9699690, if_div(V1, 17, 0, 3, To).
inspect5_2(V, send(V1,To)) :- V1 is (V+7)  mod 9699690, if_div(V1, 13, 1, 7, To).
inspect6_2(V, send(V1,To)) :- V1 is (V*7)  mod 9699690, if_div(V1, 2, 0, 4, To).
inspect7_2(V, send(V1,To)) :- V1 is (V+2)  mod 9699690, if_div(V1, 3, 1, 6, To).

if_div(V, Div, R1, _, R1) :- V mod Div =:= 0, !.
if_div(_, _, _, R2, R2).

%--------------------------------------
% monkey helpers
%--------------------------------------
monkey_number(monkey(N,_,_,_),N).
monkey_function(monkey(_,F,_,_),F).
monkey_count(monkey(_,_,Count,_),Count).
monkey_items(monkey(_,_,_,Items),Items).

monkey_with_items(Items, monkey(N,Fact,C,_), monkey(N,Fact,C,Items)).
monkey_with_items_and_count(Items, C, monkey(N,Fact,_,_), monkey(N,Fact,C,Items)).

write_monkey(M) :-
    monkey_number(M,N),
    monkey_items(M, Items),
    monkey_count(M, C),
    format("Monkey ~w: ~w inspected. ~w~n",[N,C,Items]).
