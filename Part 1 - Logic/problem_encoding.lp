#const n=6.
#const m=6.
#const k=2.
#const t=2.
#const a=2.


% There are n districts 1..n


% m companies with k experienced
company(1..k, experienced).
company(k+1..m, inexperienced).
 

team(1..t*m).

has((X/(t+1))+1,X) :- team(X).

% {has(C,T):team(T)} = t :- company(C,E).
% C1 = C2 :-  has(C1, T), has(C2, T).

district(1..n).
{req(D,T):team(T)} = a :- district(D).
D1 = D2 :- req(D1,T), req(D2,T).

% district(1..n).
% {requires(D,T):team(T)} = a :- district(D).
% D1 = D2 :-  requires(D1, T), requires(D2, T).
