echo on
clc

% Every matrix allows the factorization PA = LU. If A is square and invertible
% the result agrees with splu. In general U is an echelon matrix with nonzeros
% above the pivots.  ref removes those nonzeros and normalizes the pivots to 1.
% For random matrices P is almost sure to be I.

A = rand(2,3)
[P,L,U] = plu(A)
% press any key
pause
clc
B = rand(3,2)
[P,L,U] = plu(B)
% press any key
pause
clc
C = [zeros(2) A; B zeros(3)]
[P,L,U] = plu(C)
pause
clc

% Problem 1: Suppose A has exactly ONE nonzero entry. It is the number 7 in
% the (i,j) position. What are P and L and U? Test examples first.
%  
% Problem 2: Why does E = [A  A^2] always have the same rank as A? Compare the 
% column spaces. Then experiment to find A so that A^2 has smaller rank.
%  
% Question: For this matrix SMALL, why is the matrix U very small?

SMALL = [.0000001 .0000002; .0000003 .0000004];

[P,L,U] = plu(SMALL)
echo off
