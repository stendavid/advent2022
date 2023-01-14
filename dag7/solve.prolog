:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).
%:- use_module(library(apply)).

name(Name) --> string_without("\n", Codes), { string_codes(Name, Codes) }. 

dir(dir(Dir, Size, Subdirs)) --> 
    cd(Dir), { Dir \= ".." },
    ls(FileSize),
    sequence(dir, Subdirs),
    {
        sum_list(dir_size, Subdirs, SubdirSize),
        Size is FileSize + SubdirSize
    },
    (cd(".."); eos).

cd(Dir) --> "$ cd ", name(Dir), "\n".

ls(Size) --> "$ ls\n", sequence(ls_entry, Sizes), { sum_list(Sizes, Size) }.

ls_entry(0) --> "dir ", name(_), "\n".
ls_entry(Size) --> number(Size), " ", name(_), "\n".

sum_list(Selector, List, Sum) :- maplist(Selector, List, L), sum_list(L, Sum).

dir_size(dir(_,Size,_), Size).

all_dirsizes(dir(_,Size,Subdirs), [Size|AllSubdirs]) :- maplist(all_dirsizes, Subdirs, All0), append(All0, AllSubdirs).

% sum of the size of all directories <= 100000
solve(Sum) :-
    phrase_from_file(dir(D), 'input'),
    all_dirsizes(D, Sizes),
    include(>=(100000),Sizes,Small),
    sum_list(Small,Sum).

% total space 70000000
% size of smallest directory such that deleting it creates at least 30000000 free space
solve2(Size) :-
    phrase_from_file(dir(D), 'input'),
    D = dir(_,TotalUsed,_),
    Free is 70000000 - TotalUsed,
    Free < 30000000,
    NeededSize is 30000000 - Free,
    all_dirsizes(D, Sizes),
    include(=<(NeededSize),Sizes,CandidatesToDelete),
    min_list(CandidatesToDelete,Size).
