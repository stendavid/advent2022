:- use_module(library(func)).
:- dynamic edge/2.

edge(a,b).
edge(a,c).
edge(b,e).
edge(b,c).
%edge(c,f).
edge(e,f).


path(To, To, Visited) --> {format("path+1(~p,~p,~p)~n",[To,To,Visited])}, [To].
path(From, To, Visited) -->
    {
        edge(From, Next),
        format("path+2(~p,~p,~p)     -> edge(~p,~p)~n",[From,To,Visited,From,Next]),
        \+ memberchk(Next, Visited)
    },
    [From],
    path(Next, To, [From|Visited]).

%-------------------------------
%Iterative deepening test
%-------------------------------
% depth_first_iterative_deepening(?state,-solution)
dfs_iterative_deepening(From,To,Solution) :-
    path_(From,To,Solution,0).
    %goal(GoalNode).

path_(Node,Node,[Node],_).

path_(FirstNode,LastNode,[LastNode|Path],Depth) :-
    %format("~wFind path ~p-~p~n",[Pre,FirstNode,LastNode]),
    path_(FirstNode,OneButLast,Path, ~ is Depth+1),
    format("depth ~p~n",[Depth]),
    %format("~w..examine ~p...~p,~p~n",[Pre,FirstNode,OneButLast,LastNode]),
    edge(OneButLast,LastNode),
    \+ memberchk(LastNode,Path).
    %edge_check(OneButLast,LastNode, FirstNode,Path),
    %not_member(LastNode,Path, Pre).

edge_check(OneButLast,LastNode,FirstNode,Path) :-
    (   edge(OneButLast,LastNode)
    *->  format(" found ~p...~p,~p ~p~n", [FirstNode,OneButLast,LastNode,Path])
    ;   writeln("   ---fail"), fail).

not_member(LastNode,Path,Pre) :-
    (   \+ memberchk(LastNode,Path)
    ->  true
    ;   format("~w..membercheck failed   ---fail~n",[Pre]), fail).
