echo on
clc
% This is about the function FINDPIV, which is used by the PLU function
% to find pivots. To know what findpiv does, we type

help findpiv

% The parameter "tol", which is short for "tolerance", is used to reject
% very small pivots.  For these examples, let's use

tol = .01;

% Suppose the (1,1) entry of a matrix is zero. Then findpiv looks for the first
% entry larger than tol in that column. When the first column is all zero,
% in the pivot and below, the search goes on to the next columns.
% This matrix has only a few entries big enough to be candidates for pivots.

A = [ 0 .001 0; -.001 6 0; 6 6 0]

% press any key
pause

% The arguments 1,1 in findpiv mean that the search begins from that position
% in the matrix.  Arguments 3,4 would start in row 3 and column 4.

[r,p] = findpiv(A,1,1, tol)
[s,q] = findpiv(A',1,1, tol)

% The output is the LOCATION of the pivot, not the pivot number itself.
% Then the search for the next pivot begins from the next position down
% and to the right - after elimination has used the pivot just found.
%  
% Question 1:  If you multiply by a permutation matrix, where does the pivot
%            go to?  Define P and try findpiv( P*A ,1,1, tol).

% Question 2:  Look for a second pivot for A and also for its transpose A'.

echo off







