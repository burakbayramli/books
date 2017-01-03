echo on
clc

% The Gram-Schmidt process starts with independent vectors in the columns of A 
% It produces orthonormal vectors in the columns of Q. Example 1 is

a = [3 4]'; b = [1 2]'; A = [a b]
[Q,R] = grams(A)
% The columns of R indicate which combinations of the orthonormal q1 and q2
% lead back to a and b. Thus a = 5  times q1 plus 0 times q2.
% In matrix notation A = Q*R. 
%   
SAMEA = Q*R
% Since the transpose Q' is also the inverse, we have R = Q'*A. 
% Then the entries of R are dot products of columns of Q with columns of A.
% press any key
pause
clc
% Question 1: Does A have to be square? Try 4 by 2 and then 2 by 4:

A2 = [A;A]
[Q2,R2] = grams(A2)

% Every 2 by 4 matrix like A3 must fail because the columns of A3 are not.... 

A3 = [A A]
%press any key
pause
[Q3,R3] = grams(A3)
echo off
