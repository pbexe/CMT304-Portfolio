% name = Miles Budden
% S# = C1769331

% Instance 1:
% 1 experienced company with more than enough teams for the districts.

% USAGE:
% $ clingo problem_instance1.lp 0


#const k=1. % Number of experienced companies
#const n=2. % Number of districts
#const m=1. % Number of companies

c_ij( % Cost of a team for each company (i) for each district (j).
   % i j c
     1,1,2;
     1,2,100;
).
t_i( % Number of teams each company has.
    1,4; 
).
a_j( % Number of teams each district requires.
    1, 1;
    2, 2
).
% Achieved:
% All constants defined

#include "problem_encoding.lp".