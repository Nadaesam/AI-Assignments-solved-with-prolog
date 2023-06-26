%Test cases
%
%uninformed(3x3):uninformed_query([[[2,2,0,0,2,2,2,2,2],null]],[],G).
%uninformed(2x4):uninformed_query([[[2,2,2,2,2,0,0,2],null]],[],G).


%input code
uninformed_query(State,S,Snew):-
    uninformed_search(State,S,Snew).

%we can place all dominoes horizontally
place_domino(State,S):-
    horDomino(State,S),!.
%we can place all dominoes vertically
place_domino(State,S):-
    verDomino(State,S),!.
%we can place dominoes vertically and horizontally at the same time
place_domino(State,S):-
    horDomino(State,S),!,
    verDomino(State,S),!.

%domino placed horizontally in 3x3 board-sized

horDomino([R1,R2,R3,R4,R5,R6,R7,R8,R9],[R1,R2,R3,R4,R5,R6,R7,R8,R9]):-
    (R2=\=2,R5=\=2,R8=\=2);
    (R1=\=2,R4=\=2,R7=\=2,R3=\=2,R6=\=2,R9=\=2),!.

horDomino([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
    (R2>1;R5>1;R8>1),
    (R1>1;R4>1;R7>1;R3>1;R6>1;R9>1),
    setHorDomino([R1,R2,R3,R4,R5,R6,R7,R8,R9],State),!,
    horDomino(State,Snew).

nth2(Index, Item, List):-
  nth1(Index, List, Item).
nth2(-1, _, _).

replace1(_, _, [], []).
replace1(O, R, [O|T], [R|T2]) :-!,
       replace1(O, R, T, T2).
replace1(O, R, [H|T], [H|T2]) :- H \= O, replace1(O, R, T, T2).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

%substitute(_,[],_,[]):-!.
substitute(X,[X|T],Y,[Y|T]):-!.
    %substitute(X,T,Y,T1).
substitute(X,[H|T],Y,[H|T1]):-
    substitute(X,T,Y,T1).

%marking 2 cells horizontally as a domino tile
setHorDomino([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
    nth0(N1,[R1,R2,R3,R4,R5,R6,R7,R8,R9], Element),
    Element >1,
    Z1 is N1+1,
    nth0(Z1, [R1,R2,R3,R4,R5,R6,R7,R8,R9], Element1),
    %replace(H,EmptyTileIndex,1,T),
    %replace2(2,1,[R1,R2,R3,R4,R5,R6,R7,R8,R9],State),
    %replace2(2,1,State,S).
    %horDomino(State2,S).
    %IndexPlusOne is EmptyTileIndex -1,
    %nth2(IndexPlusOne,State,_),
    %replace(T,IndexPlusOne,1,S),

    %replace2(2,1,[R1,R2,R3,R4,R5,R6,R7,R8,R9],State),
    %replace2(2,1,State,S).
    substitute(Element1,[R1,R2,R3,R4,R5,R6,R7,R8,R9],1,State),
    %nth(N2,[H|T], 2),
    Z2 is Z1+1,
    nth0(Z2, State, 2),
    substitute(2,State,1,S).

%domino placed horizontally in 2x4 board-sized
horDomino([R1,R2,R3,R4,R5,R6,R7,R8],[R1,R2,R3,R4,R5,R6,R7,R8]):-
    (R2=\=2,R3=\=2,R6=\=2,R7=\=2);
    (R1=\=2,R4=\=2,R5=\=2,R8=\=2).

horDomino([R1,R2,R3,R4,R5,R6,R7,R8],Snew):-
    (R2=2;R3=2;R6=2;R7=2),
    (R1=2;R4=2;R5=2;R8=2),
    setHorDomino([R1,R2,R3,R4,R5,R6,R7,R8],State),
    horDomino(State,Snew).

setHorDomino([R1,R2,R3,R4,R5,R6,R7,R8],S):-
     %replace2(2,1,[R1,R2,R3,R4,R5,R6,R7,R8],State),
     %replace2(2,1,State,S).
    nth0(N1,[R1,R2,R3,R4,R5,R6,R7,R8], 2),
    Z1 is N1+1,
    nth0(Z1, [R1,R2,R3,R4,R5,R6,R7,R8], Element1),
    substitute(Element1,[R1,R2,R3,R4,R5,R6,R7,R8],1,State),
    Z2 is Z1+1,
    nth0(Z2, State, Element2),
    substitute(Element2,State,1,S).

%domino placed vertically in 3x3 board-sized

is_Empty(X):-
    X>1.

verDomino([R1,R2,R3,R4,R5,R6,R7,R8,R9],[R1,R2,R3,R4,R5,R6,R7,R8,R9]):-
    (R4=\=2,R5=\=2,R6=\=2);
    (R1=\=2,R2=\=2,R3=\=2,R7=\=2,R8=\=2,R9=\=2).

verDomino([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
    (R4>1;R5>1;R6>1),
    ((R1>1;R2>1;R3>1);(R7>1;R8>1;R9>1)),
    setVerDomino([R1,R2,R3,R4,R5,R6,R7,R8,R9],State),
    verDomino(State,Snew).

setVerDomino([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
    %replace2(2,1,[R1,R2,R3,R4,R5,R6,R7,R8,R9],State),
    %replace2(2,1,State,S).
    nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9], 2),
    Z1 is N+1,
    nth0(Z1,[R1,R2,R3,R4,R5,R6,R7,R8,R9],E1),
    is_Empty(E1),
    %nth0(Z1, [R1,R2,R3,R4,R5,R6,R7,R8,R9], E1),
    %is_empty(Element1),
    Z2 is Z1+3,
    nth0(Z2, [R1,R2,R3,R4,R5,R6,R7,R8,R9], E2),
    is_Empty(E2),
    substitute(E1,[R1,R2,R3,R4,R5,R6,R7,R8,R9],1,State),
    substitute(E2,State,1,S).

%domino placed horizontally in 2x4 board-sized
verDomino([R1,R2,R3,R4,R5,R6,R7,R8],[R1,R2,R3,R4,R5,R6,R7,R8]):-
    (R5=\=2,R6=\=2,R7=\=2,R8=\=2);
    (R1=\=2,R2=\=2,R3=\=2,R4=\=2).

verDomino([R1,R2,R3,R4,R5,R6,R7,R8],Snew):-
    (R5=2;R6=2;R7=2;R8=2),
    (R1=2;R2=2;R3=2;R4=2),
    setVerDomino([R1,R2,R3,R4,R5,R6,R7,R8],State),
    verDomino(State,Snew).

setVerDomino([R1,R2,R3,R4,R5,R6,R7,R8],S):-
    %replace2(2,1,[R1,R2,R3,R4,R5,R6,R7,R8],State),
    %replace2(2,1,State,S),
    nth0(N1,[R1,R2,R3,R4,R5,R6,R7,R8], 2),
    Z1 is N1+1,
    nth0(Z1, [R1,R2,R3,R4,R5,R6,R7,R8], Element1),
    Element1 >1,
    %substitute(Element1,[R1,R2,R3,R4,R5,R6,R7,R8],1,State),

    %Z2 is Z1+4,
    %nth0(Z2, State,2),
    %nth0(Z2,[R1,R2,R3,R4,R5,R6,R7,R8],Element2),
    Z2 is Z1+4,
    %nth0(Z2,State,Element2),
    %Element2>1,
    %replace(State,Z2,1,S).
    %substitute(Element2,State,1,S).
    nth0(Z2, [R1,R2,R3,R4,R5,R6,R7,R8], Element2),
    Element2>1,
    substitute(Element1,[R1,R2,R3,R4,R5,R6,R7,R8],1,State),
    substitute(Element2,State,1,S).

%printing solution for uninformed search
printSolution1([State, null],_):-
write(State), nl.

printSolution1([State, Parent], Closed):-
member([Parent, GrandParent], Closed),
printSolution1([Parent, GrandParent], Closed),
write(State), nl.

%uninformed search code
uninformed_search(Open, Closed, Goal):-
getState1(Open, [CurrentState,Parent], _),
CurrentState = Goal,nl,
printSolution1([CurrentState,Parent], Closed).

uninformed_search(Open, Closed, Goal):-
getState1(Open, CurrentNode, TmpOpen),
getAllValidChildren1(CurrentNode,TmpOpen,Closed,Children),
addChildren1(Children, TmpOpen, NewOpen),
append(Closed, [CurrentNode], NewClosed),
uninformed_search(NewOpen, NewClosed, Goal).



getAllValidChildren1(Node, Open, Closed, Children):-
findall(Next, getNextState1(Node, Open, Closed, Next),Children).

getNextState1([State,_], Open, Closed, [Next,State]):-
place_domino(State, Next),
not(member([Next,_], Open)),
not(member([Next,_], Closed)).

% Implementation of getState and addChildren determine the search alg.
% BFS
getState1([CurrentNode|Rest], CurrentNode, Rest).
addChildren1(Children, Open, NewOpen):-
append(Open, Children, NewOpen).
