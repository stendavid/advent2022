:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

%[C]         [S] [H]                
%[F] [B]     [C] [S]     [W]        
%[B] [W]     [W] [M] [S] [B]        
%[L] [H] [G] [L] [P] [F] [Q]        
%[D] [P] [J] [F] [T] [G] [M] [T]    
%[P] [G] [B] [N] [L] [W] [P] [W] [R]
%[Z] [V] [W] [J] [J] [C] [T] [S] [C]
%[S] [N] [F] [G] [W] [B] [H] [F] [N]
% 1   2   3   4   5   6   7   8   9 

initial_stacks(S) :- S = [
        ['C','F','B','L','D','P','Z','S'],
        ['B','W','H','P','G','V','N'],
        ['G','J','B','W','F'],
        ['S','C','W','L','F','N','J','G'],
        ['H','S','M','P','T','L','J','W'],
        ['S','F','G','W','C','B'],
        ['W','B','Q','M','P','T','H'],
        ['T','W','S','F'],
        ['R','C','N']
    ].

% move_rev(N, FromIn, ToIn, FromOut, ToOut) eg. move 2 abc xyz ->  c baxyz
move_rev(N, From, To, FromOut, ToOut) :- move_rev(N, From, To, FromOut, ToOut, []).
move_rev(0, From, To, FromOut, ToOut, MovedAcc) :- FromOut = From, append(MovedAcc, To, ToOut).
move_rev(N, [F|FromIn], ToIn, FromOut, ToOut, MovedAcc) :-
    N > 0,
    N1 is N - 1,
    move(N1, FromIn, ToIn, FromOut, ToOut, [F|MovedAcc]).

% move(N, FromIn, ToIn, FromOut, ToOut) eg. move 2 abc xyz ->  c abxyz
move(N, From, To, FromOut, ToOut) :- move(N, From, To, FromOut, ToOut, []).
move(0, From, To, FromOut, ToOut, MovedAcc) :- FromOut = From, append(MovedAcc, To, ToOut).
move(N, [F|FromIn], ToIn, FromOut, ToOut, MovedAcc) :-
    N > 0,
    N1 is N - 1,
    append(MovedAcc, [F], MovedAcc1),
    move(N1, FromIn, ToIn, FromOut, ToOut, MovedAcc1).

replace(1, X, [_,B,C, D,E,F, G,H,I], [X,B,C, D,E,F, G,H,I]).
replace(2, X, [A,_,C, D,E,F, G,H,I], [A,X,C, D,E,F, G,H,I]).
replace(3, X, [A,B,_, D,E,F, G,H,I], [A,B,X, D,E,F, G,H,I]).
replace(4, X, [A,B,C, _,E,F, G,H,I], [A,B,C, X,E,F, G,H,I]).
replace(5, X, [A,B,C, D,_,F, G,H,I], [A,B,C, D,X,F, G,H,I]).
replace(6, X, [A,B,C, D,E,_, G,H,I], [A,B,C, D,E,X, G,H,I]).
replace(7, X, [A,B,C, D,E,F, _,H,I], [A,B,C, D,E,F, X,H,I]).
replace(8, X, [A,B,C, D,E,F, G,_,I], [A,B,C, D,E,F, G,X,I]).
replace(9, X, [A,B,C, D,E,F, G,H,_], [A,B,C, D,E,F, G,H,X]).

print_matrix(Matrix):- print_matrix('', Matrix).
print_matrix(Indent, Matrix):- print_matrix(Indent, 1, Matrix).
print_matrix(_, _, []).
print_matrix(Indent, N, [Row|Rows]):-
  atomic_list_concat(Row, TRow),
  format('~w ~w ~w~n', [Indent, N, TRow]),
  N1 is N + 1,
  print_matrix(Indent, N1, Rows).

process([], Stacks, Stacks).
process([[N, From, To] | Commands], Stacks, Result) :-
    format('Process move ~w from ~w to ~w~n', [N, From, To]),
    nth1(From, Stacks, FromIn),
    nth1(To, Stacks, ToIn),
    %move_rev(N, FromIn, ToIn, FromOut, ToOut),
    move(N, FromIn, ToIn, FromOut, ToOut),
    replace(From, FromOut, Stacks, Stacks1),
    replace(To, ToOut, Stacks1, Stacks2),
    print_matrix('                         ', Stacks2),
    process(Commands, Stacks2, Result).

cmd(N, From, To) -->
    string(_),
    "move ", number(N), " from ", number(From), " to ", number(To),
    remainder(_).

read_input(Commands) :-
    findall([N,From,To], phrase_from_file(cmd(N, From, To), 'input-moves'), Commands).

solve(Result) :-
    read_input(Commands),
    initial_stacks(Stacks),
    process(Commands, Stacks, Result).

print_heads(ListOfLists) :-
    findall(H, member([H|_], ListOfLists), Heads),
    atomic_list_concat(Heads, Text),
    format("~w~n", [Text]).
