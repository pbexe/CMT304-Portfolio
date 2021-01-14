#const n=3. % Number of districts
#const m=4. % Number of companies
#const k=2. % Number of experienced companies

c_ij( % Cost of a team for each company (i) for each district (j).
   % i j c
     1,1,4;
     1,2,4;
     1,3,4;
     2,1,3;
     2,2,3;
     2,3,3;
     3,1,2;
     3,2,2;
     3,3,2;
     4,1,1;
     4,2,1;
     4,3,1;
).
t_i( % Number of teams each company has.
    1,2; 
    2,1;
    3,5;
    4,1;
).
a_j( % Number of teams each district requires.
    1, 2;
    2, 3;
    3, 1;
).
% Achieved:
% All constants defined

maxteams(X) :- X = #sum{N, T: t_i(T, N)}.
% Achieved:
% Number of teams to create is calculated.

company(1..m).
experienced(1..k).
inexperienced(k + 1..m).
% Achieved:
% m companies.
% 1...k experienced companies.
% K+1...m inexperienced companies.

team(1..M) :- M = #sum{X: maxteams(X)}.
{has(C, T): team(T)} :- company(C).
C1 = C2 :- has(C1, T), has(C2, T).
% Achieved:
% All combinations of teams amongst companies.

:- C = 1..m, TEAMS = #count{T: has(C, T), team(T)}, t_i(C, X), X != TEAMS.
% Achieved:
% A company that has the wrong amount of teams cannot exist.

district(1..n).
{req(D, T): team(T)} :- district(D).
D1 = D2 :- req(D1, T), req(D2, T).
% Achieved:
% Each district requires a team. 2 districts are the same if the same team is required by each of them.

:- D = 1..n, #count{T: experienced(C), req(D, T), has(C, T), company(C), team(T)} = 0.
% Achieved:
% Each district get at least 1 experienced team. ie: It cannot be the case that a district as 0 teams.

:- D = 1..n, TEAMS = #count{T: req(D, T), has(C, T), company(C), team(T)}, a_j(D, X), X != TEAMS.
% Achieved:
% Each district cannot have the incorrect number of teams.

cost(X, D) :- X = #sum{Y, D: req(D, T), has(C, T), c_ij(C, D, Y)}, district(D).
cost(X) :- X = #sum{C, D: cost(C, D)}.
#minimize{C, D: cost(C, D)}.
% Achieved:
% Minimize total cost.

#show cost/1. #show has/2. #show req/2.
% Achieved:
% Shows the data required to implement the optimum team division stratergy.