#const n=2.
#const m=3.
#const k=1.

companycost(1,4; % Cost of a team from each company
            2,5;
            3,3).

t_i(1,2; % Number of teams each company has
    2,1;
    3,1).

maxteams(X) :- X = #sum{N,T: t_i(T,N)}.

a_j(1, 2; % Number of teams each district requires
    2, 1).
% Achieved:
% urgh TODO: Reduce the number of constants needed

company(1..m).
experienced(1..k).
inexperienced(k+1..m).
% Achieved:
% m companies.
% 1...k experienced companies.
% K+1...m inexperienced companies.

team(1..M) :- M = #sum{X:maxteams(X)}. % TODO work out how many total teams are needed
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


cost(X,D) :- X = #sum{Y,C: req(D,T), has(C,T), companycost(C, Y)}, district(D), team(T).
cost(X) :- X = #sum{C,D: cost(C,D)}.
#minimize{C,D: cost(C,D)}.
% Achieved:
% Minimize total cost.

% #show cost/1.