% Получение N-го элемента списка

% без использования стандартных предикатов
get1([A|_],1,A).
get1([_|T],N,A) :- get1(T,N1,A), N is N1+1.

% на основе использования стандартных предикатов
get2(L,N,R) :- append(X,_,L),length(X,N),last(X,R).




