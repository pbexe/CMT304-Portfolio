weight(a,12; b,1; c,4; d,2; e,1).
value(a,4; b,2; c, 10; d,2; e,1).
#const maxweight=15.


{in(I)} :- weight(I,W).
:- #sum{W,I : in(I), weight(I,W)} > maxweight.
#maximize{V,I : in(I), value(I,V)}.


#show in/1.