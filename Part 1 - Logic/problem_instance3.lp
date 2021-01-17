% name = Miles Budden
% S# = C1769331


#const k=1. % Number of experienced companies
#const n=4. % Number of districts
#const m=3. % Number of companies

c_ij( % Cost of a team for each company (i) for each district (j).
   % i j c
     1,1,50;
     1,2,60;
     1,3,70;
     1,4,80;
     2,1,5;
     2,2,6;
     2,3,7;
     2,4,8;
     3,1,5;
     3,2,6;
     3,3,7;
     3,4,8;
     4,1,5;
     4,2,6;
     4,3,7;
     4,4,8

).
t_i( % Number of teams each company has.
    1,4; 
    2,3;
    3,10
).
a_j( % Number of teams each district requires.
    1, 2;
    2, 2;
    3, 2;
    4, 2
).
% Achieved:
% All constants defined

#include "problem_encoding.lp".