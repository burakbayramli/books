echo on
clc
% The solve program slv uses slu to factor A into L*U. The matrix A must 
% be square and invertible and it must need no row exchanges.
%  
% slv does forward elimination on the right side b to reach c in Ux = c.
% Then back substitution gives x. We give two applications and two experiments.
%  
% Example 1: Find the parabola y = F + Gt + Ht^2 which equals b = 1, 4, 3
% at the points t = 2, 3, 4. The solution vector will be x = (F,G,H).
% The matrix A will be a Vandermonde matrix.


tt = [4 9 16]'; t = [2 3 4]'; b = [1 4 3]'; A = [ones(3,1) t tt]
x = slv(A,b)
%  
%  The best parabola is -17 + 13t - 2t^2
% press any key
pause
clc
% Example 2: Find the three numbers x = (K,L,M) so that K*y(2) + L*y(3) + M*y(4)
% gives the correct integrals of the three functions y(t) = 1, t, and t^2.
%  
% The integration is from t = 2 to t = 4 so the integrals from calculus 
% are 2, 6, and 56/3. The three equations for K, L, M are
%  
% y(t) = 1          K +  L +   M = 2
% y(t) = t         2K + 3L +  4M = 6
% y(t) = t^2       4K + 9L + 16M = 56/3
%  
% The matrix for these integrals is the transpose A' of the matrix A in Example 1.
% You may recognize that K, L, M come from Simpson's Rule.
b = [2 6 56/3]; x = slv(A',b)
% Experiment 1: Choose A and b with random entries between 0 and 1. 
% The probability is ZERO that slu and slv will fail on A. 
% Experiment without theory: How large is an average answer x ?


A = rand(5); b = rand(5,1);
x = slv(A,b) 
% press any key
pause
clc
%  
% Experiment 2: Choose A to be the n by n tridiagonal -1, 2, -1 matrix.
%  
%    Choose b = [1 0 0 ... 0]'. Solve Ax = b for several values of n 
%    and find a general formula for x. Here is n = 4:

A = toeplitz([2 -1 0 0]); b = [1 0 0 0]';
x = slv(A,b)
echo off


