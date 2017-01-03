echo on
clc

% The nullspace matrix N contains the "special solutions" to Ax=0. 
% They are a basis for the nullspace. Each special solution has one free 
% variable set to 1, the other free variables set to zero. 
A = ones(3)
N = null(A)
B = 5*ones(1,3)
N = null(B)
% Why did A and B have the same nullspace? 
% press any key
pause; clc
% The 3 by 1 matrix C = B' has only x = 0 in its nullspace. The nullspace is Z!
% The dimension is zero. The matrix N is empty. The columns are independent 
% ( more exactly the column is independent! ) 
C = B'
N = null(C)
%  
% Question 1:  What do you expect for the nullspace matrix of D = [A  A]?
% press any key
pause
% Question 2:  Multiply any matrix A by a square invertible matrix M 
%              Compare the nullspace matrices N(A) and N(M*A) and explain. 

% press any key
pause; clc
% FUNDAMENTAL THEOREM:  The columns of N are orthogonal to the rows of A. 
% A * N = zero matrix.  What happens if we ask for null(N')??
% Watch the rows and also watch the ROW SPACE. 
A = [ 3 1 0 ; 4 0 1 ]

N = null (A)
B = null (N')
% press any key
pause
clc

A2 = [ 3 1 0 ; 7 1 1 ]

N2 = null (A2)
B2 = null (N2')
% Maybe the row space of A is always the column space of null(null(A)')
echo off





