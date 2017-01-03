echo on
clc
% This code uses the power of MATLAB's eig program to find eigenvalues
% of large matrices. When we only want to display eigenvalues we use eig.
%  
% Special types of matrices have eigenvalues in special positions.
% Here are experiments from a random R:
% press any key
pause
clc

R = rand(4)
eig(R)

% press any key
pause
clc

eig(R + R')

% Notice that R + R' is symmetric. Its eigenvalues were real.
% Of course R may have real eigenvalues too! Since it was random I don't know.
% press any key
pause

eig(R - R')

% This matrix R - R' is SKEW-symmetric. Its eigenvalues are pure imaginary.
% press any key
pause
clc

eig(R'*R) 

% This matrix R'*R is again symmetric. Its eigenvalues are real and POSITIVE.
% press any key
pause

eig(R*R')

% R*R' has the same eigenvalues as R'*R. But different eigenvectors!
% press any key
pause
clc

% You can create matrices with desired eigenvalues 1,2,3,4 from any') 
% invertible S times lambda times S inverse. 

S = rand(4);
lambda = diag([1 2 3 4]);
A = S * lambda * inv(S);
eig(A)

% press any key
pause
clc
% Finally remember that A*A has the same S but the eigenvalues are squared.
% So the following matrix should be the zero matrix.

B = A * A - S * lambda * lambda * inv(S)

% Question 1: What is the eigenvector matrix for the transpose A'?
%             Its eigenvalue matrix is easy.
%  
% Question 2: Create a Markov matrix M, positive with columns adding to 1.
%  Use eig(M) to predict its steady state. One eigenvalue should equal 1.
% Compute M to the 16th power directly, and also from S*lamb^16*inv(S).

% News has just come that the pascal matrices have unusual eigenvalues.
% If e is an eigenvalue so is 1/e. It is det(A - e*I) that is "symmetric".
% The next pages show the eigenvalues for pascal(4) and pascal(6).
% press any key
pause
clc

A = pascal(4)
eigen(A)

% press any key for another pascal example. Look at det(P - e*I):
pause
clc
P = pascal(6)
eigen(P)
echo off


