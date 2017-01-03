echo on
clc

% This code gives the eigenvalues and eigenvectors of a 2 by 2 matrix.
% The lambda's are the roots of a second-degree (quadratic) polynomial.
%  
% We also test A*A and 5A and A + 5I and A inverse:
A = [2 2; -1 5]
eigen2(A)
% press any key
pause
clc
A*A
eigen2(A*A)
% press any key
pause
clc
5*A
eigen2(5*A)
% press any key
pause
clc
A + 5*eye(2)
eigen2(A + 5*eye(2))
% press any key
pause
clc
inverse(A)
eigen2(inverse(A))
% press any key
pause
clc
%  Symmetric, skew-symmetric and orthogonal matrices have perpendicular
%  eigenvectors. Note that A + A' is symmetric and A - A' is skew-symmetric:
A + A'
eigen2(A + A')
% press any key
pause
clc
A - A'
eigen2(A - A')
% press any key
pause
clc
% Gram-Schmidt will produce an orthogonal matrix Q. 
% Its eigenvalues should have absolute value 1.
[Q,R] = grams(A);
Q 
eigen2(Q)
absolute_value = sqrt(0.8944^2 + 0.4472^2)
% press any key
pause
clc
R = R
eigen2(R)
% Question 1: Do the eigenvalues of AB equal the eigenvalues of BA? Try examples
% Question 2: What eigenvectors come from eigen2 when A does not have
%             2 independent eigenvectors? Does eigen(A) give the same output?
% Question 3: Are random 2 by 2 matrices more likely to have real or complex
%             eigenvalues?
echo off










