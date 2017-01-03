echo on
clc
% Choose a matrix with zeros on its diagonal: 
A = ones(3) - eye(3)
% Is this matrix invertible ? 
%  
% press any key to look for its inverse
pause
clc
B = inverse(A)
% Does A have an LU factorization ? 
%  
% press any key
pause
clc
[L,U] = slu(A)
% Does PA have an LU factorization (for which permutation)?
%  
% Press any key
pause
clc
[P,L,U] = splu(A)
permuteA = P*A
permuteA - L*U
echo off
