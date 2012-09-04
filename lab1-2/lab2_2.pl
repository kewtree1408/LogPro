% Устанавливаем имена и фамилии
people(X):-
      X = [leonid, michael, nikolay, oleg, petr].
surname(Y):-
      Y = [atarov, bartenev, klenov, danilin, ivanov].

%Возвращает друзей
get_friends(X, Name, Solve) :-
      people(Tmp),
      nth0(Pos, Tmp, Name), % на каком месте стоит человек 
      nth0(Pos, Solve, X).

% Количество друзей (Res) у Name
count(Name, Res, Solve) :-
      get_friends(Friends, Name, Solve),
      length(Friends, Res).

% знакомые -- т.е. у Name1 одним из друзей является Name2. Наоборот тоже верно.
know(Name1, Name2, Solve) :-
      get_friends(Friends1, Name1, Solve),
      get_friends(Friends2, Name2, Solve),
      member(Name2, Friends1),
      member(Name1, Friends2).

%ускорение -- накладывает условия на фамилии которые очевидны
speedup(Who) :-
      [Atarov, Bartenev, Klenov, Danilin, Ivanov] = Who,

%       Klenov \= nikolay,
%       Klenov \= oleg,
%       Klenov \= michael,
%       Klenov \= petr,

      % =>
      Klenov == leonid, % Только один из всех знаком с Кленовым, аналогично тому, что Кленов знаком только с одним из всех (А у нас только Леонид знаком с одним из всех)

      Ivanov \= nikolay, % Иванов и Николай знакомы (т.е. разные люди)
      Danilin \= michael, %Данилин и Михаил незнакомы (т.е. разные люди)
      Bartenev \= petr. %Кол-во друзей у Бартенва не совпадает с колв-ом друзей у Петра


% Возвращает список имен на нужном месте в соответсвии с фамилиями
info(Who, Solve) :-
      [Atarov, Bartenev, Klenov, Danilin, Ivanov] = Who,

      count(Bartenev, 2, Solve),
      count(petr,     3, Solve),
      count(leonid,   1, Solve),
      not(know(Danilin, michael, Solve)),
      know(nikolay, Ivanov, Solve),

      know(nikolay, michael, Solve),
      know(nikolay, oleg, Solve),
      know(michael, oleg, Solve),

      count(Atarov,   3, Solve),
      count(Klenov,   1, Solve).

%Все комбинации без повторений
combinations(_,[]).
combinations([X|T],[X|Comb]) :- combinations(T,Comb).
combinations([_|T],[X|Comb]) :- combinations(T,[X|Comb]).

% список друзей для одного человека
get_friendlist(Res, Man) :-
      people(P0),
      delete(P0, Man, P1),
      combinations(P1, Res).

print([],[]).
print([H1|T1],[H2|T2]) :- write(H1), write(' '), write(H2), write('\n'), print(T1,T2).

solve(X) :-
      people(S0), % S0 - начальный порядок имен, WhoIsWho -- переставленный
      permutation(S0, WhoIsWho),

      speedup(WhoIsWho),

      people([X1,X2,X3,X4,X5]),

      get_friendlist(R1, X1),
      length(R1, 1), % leonide

      get_friendlist(R5, X5), % petr
      length(R5, 3),

      get_friendlist(R2, X2), % michael
      member(oleg, R2),
      member(nikolay, R2),

      get_friendlist(R3, X3), % nikolay
      member(oleg, R3),       
      member(michael, R3),

      get_friendlist(R4, X4), % oleg
      member(michael, R4),
      member(nikolay, R4),

       Solve = [R1, R2, R3, R4, R5],

       info(WhoIsWho, Solve),
       foreach_friend(R1, X1, Solve),
       foreach_friend(R2, X2, Solve),
       foreach_friend(R3, X3, Solve),
       foreach_friend(R4, X4, Solve),
       foreach_friend(R5, X5, Solve),

      write('SOLVE:\n'),
      surname(Sn),
      print(WhoIsWho,Sn),
      write([R1, R2, R3, R4, R5]),
      write('\n'),

      X = WhoIsWho.

foreach_friend([],_,_).
foreach_friend([H|T], Name, Solve) :- know(H, Name, Solve), foreach_friend(T, Name, Solve).
