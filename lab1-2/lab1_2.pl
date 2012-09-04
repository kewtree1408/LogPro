lower([],[_|_]).
lower([X|_],[Y|_]) :- X<Y.
lower([A|X],[A|Y]) :- lower(X,Y).

equal([],[]).
equal(A,A).
equal([A|X],[A|Y]) :- equal(X,Y).

bigger([_|_],[]).
bigger([X|_],[Y|_]) :- X>Y.
bigger([A|X],[A|Y]) :- bigger(X,Y).

cmp(A,B,N) :- lower(A,B), N = 'lower'.
cmp(A,B,N) :- equal(A,B), N = 'equal'.
cmp(A,B,N) :- bigger(A,B), N = 'bigger'.
