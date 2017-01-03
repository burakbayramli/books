echo on
clc

% Start with random matrices and project onto their column spaces.
%  
P = projmat(rand(3,4))
P = projmat(rand(2,2))
%  Why did you get P = I (if you did) ?
%  The formula is P = A*inverse(A'*A)*A'.
%  There may be a better way to see why P = I.

% press any key
pause
clc

A = rand(3,2)
P = projmat(A)
% Try squaring this matrix to see if you get back P.

P*P - P
% Try multiplying P times A to see if you get back A.

P*A - A
%  
% Question: Find the projection P*b of b = [1 0 0]' onto the column space of A.
% Also use lsq(A,b) to find the best combination x of the columns.
% Now compute  p = A*x. This should give the same as p = P*b.
echo off
