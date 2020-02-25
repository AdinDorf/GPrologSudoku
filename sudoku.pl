%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Project #: 4
% Group Names: Christian Alfaro, Matthew Byler, Adin Dorf, and Ethan Gilchrist
% Course: CSE 259, MW 12:15-1:30
% Description: This program provides a sudoku solving tool.
%              It is based off of the following github repo https://gist.github.com/luan/1995582 by Luan. His program implemented a simple Sudoku Solver which made limited use of the built-in Finite Domain Library in GProlog.
%              Because of this his program ran slow. We studied the Finite Domain Library and examples of its applications in SWI Prolog Sudoku implementation in order to improve the efficiency of Luan's algorithm.
%              A major addition was the introduction of nonet checking which is capable of comparing blocks 9 pieces on the board in order to quickly meet constraints. This brings down execution time significantly!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


main :- repeat,
    write('1. How does this program work?'),nl,
    write('2. Solve a board'),nl,
    write('3. Try built-in boards'),nl,
    write('4. Quit'),nl,
    write('Enter your choice:'),nl,
    read(Choice), Choice>0, Choice =<4,
    execute(Choice), Choice = 4, !.

printFormatting :-
    nl, write('To solve a sudoku board please enter a list in the following format (or copy this one):'),nl,
    write('[[_,4,3,_,8,_,2,5,_],'),nl,
    write('[6,_,_,_,_,_,_,_,_],'),nl,
    write('[_,_,_,_,_,1,_,9,4],'),nl,
    write('[9,_,_,_,_,4,_,7,_],'),nl,
    write('[_,_,_,6,_,8,_,_,_],'),nl,
    write('[_,1,_,2,_,_,_,_,3],'),nl,
    write('[8,2,_,5,_,_,_,_,_],'),nl,
    write('[_,_,_,_,_,_,_,_,5],'),nl,
    write('[_,3,4,_,9,_,7,1,_]].'),nl,nl.


solveBoard :-
    nl, write('Enter a board to solve:'),nl,
    read(List),nl,
    sudokuNew(List),nl,
    nl, write('Solution'),nl,
    printResults(List),nl.

builtInBoard :-
    nl, write('Options:'),nl,
    write('1. Standard Board'),nl,
    write('2. 1 Missing'),nl,
    write('3. 6 Missing'),nl,
    write('4. 1 Piece On The Board'),nl,
    write('5. Impossible Board - Wikipedia Board Designed To Work Against The Program'),nl,
    write('6. Failure'),nl,
    read(BoardChoice), BoardChoice>0, BoardChoice =<6,
    tryABoard(BoardChoice), !.

execute(1):-
    printFormatting.
execute(2):-
    solveBoard.
execute(3):-
    builtInBoard.
execute(4):-
    write('Quitting program. Happy Prolog.').

tryABoard(1) :-
    nl,testStandard,nl.
tryABoard(2) :-
    nl,test1Missing,nl.
tryABoard(3) :-
    nl,test6Missing,nl.
tryABoard(4) :-
    nl,test1Piece,nl.
tryABoard(5) :-
    nl,testImpossible,nl.
tryABoard(6) :-
    nl,testFailure,nl.



rows([  % rows was a part of the original program and here is only left to represent the structure of the sudoku "board"
          [X11, X12, X13, X14, X15, X16, X17, X18, X19],
          [X21, X22, X23, X24, X25, X26, X27, X28, X29],
          [X31, X32, X33, X34, X35, X36, X37, X38, X39],
          [X41, X42, X43, X44, X45, X46, X47, X48, X49],
          [X51, X52, X53, X54, X55, X56, X57, X58, X59],
          [X61, X62, X63, X64, X65, X66, X67, X68, X69],
          [X71, X72, X73, X74, X75, X76, X77, X78, X79],
          [X81, X82, X83, X84, X85, X86, X87, X88, X89],
          [X91, X92, X93, X94, X95, X96, X97, X98, X99]
        ],
        [
          [X11, X12, X13, X21, X22, X23, X31, X32, X33],
          [X41, X42, X43, X51, X52, X53, X61, X62, X63],
          [X71, X72, X73, X81, X82, X83, X91, X92, X93],
          [X14, X15, X16, X24, X25, X26, X34, X35, X36],
          [X44, X45, X46, X54, X55, X56, X64, X65, X66],
          [X74, X75, X76, X84, X85, X86, X94, X95, X96],
          [X17, X18, X19, X27, X28, X29, X37, X38, X39],
          [X47, X48, X49, X57, X58, X59, X67, X68, X69],
          [X77, X78, X79, X87, X88, X89, X97, X98, X99]
        ]).

% Original Implementation!

% transpose([[A]],[[A]]).
% transpose([[A|H]|T],[[A|HH]|TT]):-
%    set_heads(T,HH,R),
%    set_heads(TT,H,RR),
%    transpose(R,RR).

% set_heads([],[],[]).
% set_heads([[H|T]|L],[H|TT],[T|R]):-
%    set_heads(L,TT,R).

% all_different_list([]).
% all_different_list([H|T]) :-
%   fd_all_different(H),
%   all_different_list(T).

% domain(1).
% domain(2).
% domain(3).
% domain(4).
% domain(5).
% domain(6).
% domain(7).
% domain(8).
% domain(9).

% domain_array([]).
% domain_array([H|T]) :-
%   domain(H),
%   domain_array(T).

% domain_matrix([]).
% domain_matrix([H|T]) :-
%   domain_array(H),
%   domain_matrix(T).

% sudoku(Rows) :-
%   domain_matrix(Rows),

%   transpose(Rows, Columns),
%   squares(Rows, Squares),

%   all_different_list(Rows),
%   all_different_list(Columns),
%   all_different_list(Squares).

sudokuNew(Rows) :-
    maplist(valid1To9, Rows), % map list is going to check if the domain is 1-9 for the entire board. Makes use of FD library.
    maplist(fd_all_different, Rows), % check if the rows are different from each other using FD library. Maplist here will check if all different.
    transpose(Rows,Cols), % see function. It will transpose to all necessary constraints
    maplist(fd_all_different, Cols), % again use map list for checking if different on the cols
    nonets(Rows,Nonets),
    maplist(fd_all_different, Nonets), % again use map list for checking if different on the nonets
    maplist(fd_labeling, Rows).


 
valid1To9(Rows) :- fd_domain(Rows,1,9). % have to wrap the domain 1-9 call here because you can't use functions as parameters

 
% new method to improve speed
% implements checking of nonet (ie nonets of 9) to see if there are repeats in them
nonets(Rows,Nonets) :-
    maplist(splitInto3,Rows,Xs), % map the split operation to rows and Xs. Here Xs represents a count of block positions
    transpose(Xs,Ys), % here Xs is transposed to form Ys
    concat(Ys,Zs), % concatenate Ys to Zs
    concat_map(splitInto3,Zs,Nonets). % the split 3 operation is going to break each row into 3 value block 3 times. These are represented with Xs Ys Zs which are transposed and concatenated to create nonets 

splitInto3([X,Y,Z|L],[[X,Y,Z]|R]) :-
    splitInto3(L,R). % split 3 will split the row into x y z three times. Ie [x1,y1,z1] [x2,y2,z2] [x3,y3,z3] then we can craft the Xs Ys Zs
splitInto3([],[]). % Recursively repeat until the list is empty
 
 
% here we do a combination of the three vectors into a nonet
concat_map(F,Xs,Ys) :-
    call(F,Xs,Zs),  % call will  will check if the appending of Xs and Zs has been done correctly
    maplist(concat,Zs,Ys).  % map the concatentation operation to all Zs and Ys.
% call(Closure, Arg1,…, ArgN) calls the goal call(Goal) where Goal is constructed by appending Arg1,…, ArgN (1 ≤ N ≤ 10) additional arguments to the arguments (if any) of Closure.
 
concat([],[]). % recursive stopping condition
% First given the X list with the first element selected and append to a Ys list
concat([X|Xs],Ys) :- % concatenate. The first element of X and a list of Ys are the parameter (though it used on different types ie Zs)
  append(X,Zs,Ys), % append first element x to list Ys and store in Zs
  concat(Xs,Zs). % and then continue appending to X to Zs until reaching end of Xs and Ys
 
% we improve the transpose function by implementing a bind_head operation in order to bind the head to the row and columns constructing appropriate results
transpose([[A]],[[A]]). % A represents the current value
transpose([[A]|Col], [[A|Row]]) :-
    transpose(Col,[Row]). % if A is a member of the row, tranpose the row into a column

transpose([[A|Row]], [[A]|Col]) :-
    transpose([Row],Col). % if A is a member of the column, tranpose the column into a row

transpose([[A|Row]|Xs],[[A|Col]|Ys]):- % if A is a member of the row for the given Xs and a member of the Column for the given Ys we will...
% use bind_head to transpose the rows and columns in order to avoid this repetition. This is the ultimate let's check for dupliactes in rows and columns
    maplist(bind_head, Row, Ys, CR), % here we bind the head of the row to the columns
    maplist(bind_head, Col, Xs, RC), % here we bind the head of the row to the columns
    transpose(RC,CR).

bind_head(Head,[Head|Tail],Tail). % bind the head to the tail
bind_head([],[],[]). % stopping condition

printResults([]).
printResults([H|T]) :-
  write(H),nl,
  printResults(T).


testStandard :-
  L = [ % board is missing a stadnard amount of values for a sudoku board
         [_,6,_,1,_,4,_,5,_],
         [_,_,8,3,_,5,6,_,_],
         [2,_,_,_,_,_,_,_,1],
         [8,_,_,4,_,7,_,_,6],
         [_,_,6,_,_,_,3,_,_],
         [7,_,_,9,_,1,_,_,4],
         [5,_,_,_,_,_,_,_,2],
         [_,_,7,2,_,6,9,_,_],
         [_,4,_,5,_,8,_,7,_]
  ],

    sudokuNew(L),
    printResults(L).

test1Missing :-
L = [ % 1 values missing
        [8,6,1,7,9,4,3,5,2],
        [3,5,2,1,6,8,7,4,9],
        [4,9,7,2,5,3,1,8,6],
        [2,1,8,9,7,5,6,3,4],
        [6,7,5,3,4,1,9,2,8],
        [9,3,4,6,8,2,5,1,7],
        [5,2,6,8,1,9,4,7,3],
        [7,4,3,5,2,6,8,9,1],
        [1,8,_,4,3,7,2,6,5]
  ],

  sudokuNew(L),
  printResults(L).

test6Missing :-
L = [ % 6 values missing
        [8,6,1,_,9,_,3,5,2],
        [3,5,2,1,6,8,7,4,9],
        [4,9,7,2,5,3,1,8,6],
        [2,1,8,9,7,_,6,3,4],
        [6,7,5,3,4,1,9,2,8],
        [9,_,4,6,8,2,5,1,7],
        [5,2,6,_,1,9,4,7,3],
        [7,4,3,5,2,6,8,9,1],
        [1,8,_,4,3,7,2,6,5]
  ],

    sudokuNew(L),
    printResults(L).

test1Piece :-
  L = [  % board has 1 piece
        [_,4,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_,_]
  ],

  sudokuNew(L),
  printResults(L).

testImpossible :-
  L = [ % test a wikipedia board that is supposed to be break sudoku solvers
        [_,_,_,_,_,_,_,_,_],
        [_,_,_,_,_,3,_,8,5],
        [_,_,1,_,2,_,_,_,_],
        [_,_,_,5,_,7,_,_,_],
        [_,_,4,_,_,_,1,_,_],
        [_,9,_,_,_,_,_,_,_],
        [5,_,_,_,_,_,_,7,3],
        [_,_,2,_,1,_,_,_,_],
        [_,_,_,_,4,_,_,_,9]
  ],

  sudokuNew(L),
  printResults(L).

testFailure :-
L = [ % 1 values wrong
        [8,6,1,7,9,4,3,5,2],
        [3,5,2,1,6,8,7,4,9],
        [4,9,7,2,5,3,1,8,6],
        [2,1,8,9,7,5,6,3,4],
        [6,7,5,3,4,1,9,2,8],
        [9,3,4,6,8,2,5,1,7],
        [5,2,6,8,1,9,4,7,3],
        [7,4,3,5,2,6,8,9,1],
        [1,8,8,4,3,7,2,6,5]
  ],

  sudokuNew(L),
  printResults(L).