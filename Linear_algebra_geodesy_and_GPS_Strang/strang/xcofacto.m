echo on
clc
% Cofactor (A,i,j) is (+1 or -1) times the determinant of A when row i and
% column j are removed. The sign is +1 when i+j is even.
% These cofactors go into the complete matrix cofactor(A).
% Then A times the TRANSPOSE of cofactor(A) gives an important result.
% Let's see what it is.  Here is a symmetric 3 by 3 example:

A = ones(3) + 2*eye(3)

% The matrix of cofactors is:

C = cofactor(A)

% Let's multiply A times the transpose of C.

A*C'

% It's diagonal!  What is that diagonal entry?

det(A)

% Press any key
pause
clc

% The next example is the lower triangular part of A.

A = tril(A)

% The matrix of cofactors is upper triangular!

C = cofactor(A)

% Again, multiply A by the transpose of the cofactor matrix.

A*C'

% Diagonal again.  What's the diagonal element?
% Look at A.  Can you read off its determinant?

% Press any key
pause
clc

% We have seen two examples where A*(cofactor(A))' is determ(A)*I.
% So the inverse of A is:
%
%                inv(A) = cofactor(A)'/determ(A)
%


% Question 1: When is the cofactor matrix equal to the inverse matrix?
% Answer:  If A is symmetric with determinant 1, and also if ...
%  
% Question 2A: When you change row 1 of A, which entries in cofactor(A)
%              are NOT changed?

% Question 2B: Which entries in inverse(A) are not changed?
%  
% Question 3: How much should you subtract from the last entry of a matrix
%             to make it singular?

% Answer 1: Subtract the last pivot.
% Answer 2: Subtract the determinant divided by the last cofactor.
%  
% Test to see if these answers are both right.
echo off
