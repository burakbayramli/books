echo on
clc

% slu  factors A = L*U when A is a square matrix that needs no row exchange.
A = eye(5); B = ones(5);

% The sum of A = I and B = all ones is

A + B
% Now find the factors in A + B = LU

[L,U] = slu(A + B)

% THIS MATRIX L HAS A REMARKABLE INVERSE
% To see it, go to the file  xinverse 
%  
% press any key
pause
clc
% Why does the code not work for B ? NaN means Not a Number
B = ones(5)
[L,U] = slu(B)
%  
% press any key
pause
clc
% It is important that if T is tridiagonal - only 3 nonzero diagonals
% in the center - then its factors L and U have 2 diagonals. Choose a random
% T with zeros produced by entrywise multiplication .* by TRI.
%  
%  
TRI = [1 1 0 0;1 1 1 0;0 1 1 1;0 0 1 1];
T = rand(4).*TRI
[L,U] = slu(T)
%  
% press any key to see what happens for TRI.  But splu(TRI) is fine!
pause
clc
TRI = TRI
[l,u] = slu(TRI)
% press any key 
pause
clc

% slu is different from MATLAB's lu program, which chooses 
% the largest available pivot in each column. 
% Thus lu is more likely to need P!  Compare L and U with l and u:

A = [1 2; 3 4]
[L,U] = slu(A)
[l,u] = lu(A)

echo off














