% name = Miles Budden
% S# = C1769331


#const k=1. % Number of experienced companies
#const n=2. % Number of districts
#const m=2. % Number of companies

c_ij( % Cost of a team for each company (i) for each district (j).
   % i j c
     1,1,10;
     1,2,11;
     2,1,1;
     2,2,2
).
t_i( % Number of teams each company has.
    1,2; 
    2,2
).
a_j( % Number of teams each district requires.
    1, 2;
    2, 2
).
% Achieved:
% All constants defined

#include "problem_encoding.lp".