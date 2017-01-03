echo on
clc

% The command p = randperm(5) produces the numbers 1, 2, 3, 4, 5 in a random
% order. There are 5! = 120 possible orders with equal probabilities. 

% The matrix P = I( p ,:) is the permutation matrix with the rows of I in the 
% order given by p.  Then p times the matrix P should give back [1 2 3 4 5]:

p = randperm(5)
I = eye(5);
P = I( p ,:)
p*P
% Why does this happen ?
% Press any key for the next question
pause
clc

%  If you apply elimination to P, what is the order of the pivot rows?
%  Is it always given by p ? Remember what the permutation was:
p
[P,L,U] = plu(P)
% Press any key 
pause
clc

% Experiment: Start with the vector v = [1 0 0 0 0]'. Compute Pv and PPv and 
% PPPv and ... until you come back to v. Find the smallest n so that P to the
% nth power times v is v. This n is the length of the "cycle" or "loop".

% How do you know that you will come back to v? 
% Why is 5 the maximum cycle length (and not 120 - the number of p's) ?
% Find a p that has cycle length n = 5.

% Challenge question: Find the average value of the cycle length n.

% Finally, choose a column of I that is not in the cycle. Call it w. Compute
% the vectors Pw and PPw and ... until you come back to w (a new cycle).
% 
% Every  P is a product of cycles: P = vcycle * wcycle * (other cycles)
% How many cycles go into this particular P ? 



echo off











