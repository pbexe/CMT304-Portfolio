% Consider the following situation:
% The public works division in a region has the responsibility to subcontract
% work to private companies. There are several types of tasks. Each task is carried
% out by a team, but each team is capable of carrying out all different types of tasks.
% The region is divided into districts, and the amount of tasks to be done in each
% district is known. In particular, the following information is available:
%     • The region is divided into n districts.
%     • There are m private companies such that 1 . . . k are experienced and
%       k + 1 . . . m are non-experienced.
%     • Each company i has ti teams available, for all 1 ≤ i ≤ m.
%     • Each district j requires aj many teams, for all 1 ≤ j ≤ n.
%     • The yearly cost of allocating a team from a company i to a district j is (the
%       integer) ci,j, for all 1 ≤ i ≤ m, 1 ≤ j ≤ n.
% The goal is to write a logic program for helping the public works division with this process.
% Using the information above, the program should determine the number of teams from each
% company to allocate to each district such that the following constraints are satisfied.
%     • At least one experienced company must be allocated to each district (as precaution in
% case some difficult task arises in that district).
%     • Enough teams must be allocated to meet the demand in each district.
%     • No company can be asked to provide more teams than it still has available.
%     • The cost must be minimised.
% 
% 1. Write a logic program in ASP (problem encoding.lp) which finds all solutions to the
% problem, given n, m, k, ti, aj, ci,j for all 1 ≤ i ≤ m, 1 ≤ j ≤ n. Document your code
% so the following is clear.
%     (a) How it should be used.
%     (b) What the approach to solving the problem is. In particular, you need to explain
%         what each rule achieves and how the rule achieves it.
%         Include your name and student id in the comments.
% 2. Write three problem instances (problem instancei.lp, for all i ∈ {1, 2, 3}) to test your
%    program. Document your code so it is clear what the instance is modeling.


#const k=2. % Number of experienced companies
#const n=3. % Number of districts
#const m=4. % Number of companies

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

allocate(C,D,X) :- D=1..n, req(D,T), has(C,T), X = #count{T, req(D,T), has(C,T)}. % Maybe correct?

% #show allocate/3.
% Achieved:
% Shows the data required to implement the optimum team division stratergy.