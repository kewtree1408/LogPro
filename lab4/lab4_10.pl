% вспомогательные предикаты

remove(X, [X|T], T):- !.
remove(X, [A|T], [A|Y]):- remove(X, T, Y).  

in_string([], R, R):- !.
in_string(L, R, Res):- 
	append([H1], T1, L),	
	atom_concat(R, H1, H),
	remove(H1, L, L1),
	in_string(L1, H, Res).

string(A, B):- A =.. B. 

replace(X, Y,  [X|T], [Y|T]):- !.
replace(X, Z,  [A|T], [A|Y]):- replace(X, Z, T, Y).  

% словари
%agent(active, passive).

agent_list(['Sasha', 'Ira']).
object_list([igrushki, kubiki, myachi, stixi, prosu, pieci, kukol]).
action_list([lubit]).

printElem(_, _, []):-!.
printElem(Ac, Ag, Obj):-
	append([O], T, Obj),
	string(Res, [Ac, Ag, O]),
	print(Res), print('\n'),
	printElem(Ac, Ag, T).

myPrint([], _, _):-!.
myPrint(Ac, Ag, Obj):-
	 append([A1], NewAc, Ac),
	 append([O1], NewObj, Obj), 
	 printElem(A1, Ag, O1),
	 myPrint(NewAc, Ag, NewObj). 

%Грамматика

% PHRASE -> agent Ph (1)
% PH -> PH i PH (2) | PH , no PH (3) | PH , a PH (4) |ACTION OBJECT (5)
% ACTION -> ne action (6)| action (7)
% OBJECT -> OBJECT i OBJECT (8)| object (9)

% (1)
a_phrase(P):-
	append([A], Ph, P),
	agent_list(L),
	sublist([A], L),
	a_ph(Ph, Res2, Res3), 
	myPrint(Res2, A, Res3), !.

% 2, 3, 4
a_ph(Ph, [R1, R3], [R2, R4]):-
	append(P1, T, Ph),
	append(['i'], P2, T),
	a_ph(P1, R1, R2),
	a_ph(P2, R3, R4).
% 5
a_ph(Ph, Res1, Res2):-
	append(A, O, Ph),
	a_action(A, Res1),
	a_object(O, Res2).
% 6
a_action(A, Res):-
	append(['ne'], [A1], A),
	action_list(L),
	sublist([A1], L),
	in_string(['ne_', A1], '', Res).
% 7
a_action([A], A):-
	action_list(L),
	sublist([A], L).
% 8
a_object(O, Res):-
	append(O1, T, O),
	append(['i'], O2, T),
	a_object(O1, Res1),
	a_object(O2, Res2),
	append(Res1, Res2, Res).
% 9
a_object(O, O):-
	object_list(L),
	sublist(O, L).


del(L, R):- remove(',', L, R1), del(R1, R).
del(L, L).

f(A, B, X, Y):- replace(A, B, X, Y), !.
f(A, B, X, X).

decompose(Expr):-
	del(Expr, Expr1),
	f('a', 'i', Expr1, Expr2), 
	f('no', 'i', Expr2, Expr3), % преобразование "а","но" к "и"
	a_phrase(Expr3),!.

test1(['Sasha', lubit, igrushki, ',', no, ne, lubit, kubiki, i, myachi]).
test2(['Ira', ne, lubit, stixi, i, prosu, ',', a, lubit, pieci]).
test3(['Ira', ne, lubit, kukol, i, igrushki, i, lubit, kubiki, i, myachi]).
