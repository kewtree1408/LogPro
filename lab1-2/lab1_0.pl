length1([],0).
length1([_|T],N) :- length1(T,N1), N is N1+1.

member1(A,[A|_]).
member1(A,[_|T]) :- member1(A,T).

append1([],X,X).
append1([A|T],X,[A|Z]) :- append1(T,X,Z).

remove1(X,[X|T],T).
remove1(X,[A|T],[A|Y]) :- remove1(X,T,Y).

permute1([],[]).
permute1(L,[X|T]) :- remove1(X,L,R),permute1(R,T).

sublist1(L,S) :- append1(_,L1,L), append1(S,_,L1).

last1(L,X) :- append1(_,[X],L).

remove_n(L,X,N) :- append1(X,Y,L), length1(Y,N).

