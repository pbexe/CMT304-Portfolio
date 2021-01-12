#const n=2.
#const m=2.
#const k=1.
#const t=2.
#const a=2.
#const c=1000000.

company(1..m).
experienced(1..k).
inexperienced(k+1..m).
% Achieved:
% 1...k experienced companies
% K+1...m inexperienced companies

team(1..t*m).
has((X/(t+1))+1,X) :- team(X).
% Achieved:
% t teams for m companies divided equally

district(1..n).
{req(D,T):team(T)} = a :- district(D).
D1 = D2 :- req(D1,T), req(D2,T).
% Achieved:
% Each district requires a teams

:- D = 1..n, #count{T: experienced(C),req(D,T),has(C,T),company(C),team(T)} = 0.
% Achieved:
% Each district get at least 1 experienced team