% name = Miles Budden
% S# = C1769331

% USAGE:
% This file contains the logic for the test cases to run and should not be run directly.

maxteams(X) :- X = #sum{N, T: t_i(T, N)}.
% Achieved:
% Number of teams to create is calculated. Sums the total given by t_i.

company(1..m).
experienced(1..k).
inexperienced(k + 1..m).
% Achieved:
% m companies.
% 1...k experienced companies.
% K+1...m inexperienced companies.

team(1..M) :- M = #sum{X: maxteams(X)}.
% M is calculated from the maxteams atom.
{has(C, T): team(T)} :- company(C).
% Generate every combination of company-team relationships
C1 = C2 :- has(C1, T), has(C2, T).
% If 2 companies have the same team, they must be the same company.

% Achieved:
% All combinations of teams amongst companies.

:- C = 1..m, TEAMS = #count{T: has(C, T), team(T)}, t_i(C, X), X != TEAMS.
% Achieved:
% A company that has the wrong amount of teams cannot exist.

district(1..n).
% Generate 1 to n districts
{req(D, T): team(T)} :- district(D).
% Generate every combination of district-team relationships
D1 = D2 :- req(D1, T), req(D2, T).
% If 2 districts have the same team, they must be the same district.

% Achieved:
% Each district requires a team. 2 districts are the same if the same team is
% required by each of them.

:- D = 1..n, #count{T: experienced(C), req(D, T), has(C, T), company(C), team(T)} = 0.
% Achieved:
% Each district get at least 1 experienced team. ie: It cannot be the case that
% a district as 0 teams.

:- D = 1..n, TEAMS = #count{T: req(D, T), has(C, T), company(C), team(T)}, a_j(D, X), X != TEAMS.
% Achieved:
% Each district cannot have the incorrect number of teams.

allocate(C,D,T) :- company(C), district(D), req(D,T), has(C,T).
% Achieved:
% Work out which companies allocate which teams to which districts.

allocate_agg(C,D,X) :- X = #count{T: allocate(C,D,T)}, district(D), company(C).
% :- C=1..m, D=1..n, allocate_agg(C,D,X), X=0.
% Achieved:
% Sum the company-district-team alloccations as per the question.

cost(X, D) :- X = #sum{Y: req(D, T), has(C, T), c_ij(C, D, Y)}, district(D).
% The cost per district.
#minimize{C, D: cost(C, D)}.
% Achieved:
% Minimize total cost.

#show allocate_agg/3.
% Achieved:
% Shows the data required to implement the optimum team division stratergy.