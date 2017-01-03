echo on
clc
% A basis consists of independent vectors whose combinations produce the whole
% subspace. Different combinations of basis vectors  produce different vectors!
% basis.m  knocks out dependent columns to leave a basis for the column space.

u = [1 3 0]'; v = [2 1 1]'; w = rand(3,1);

A = [u  v  u  w]

B  = basis(A)

% press any key
pause
clc
A2 = [u  u+v  u+2*v  u+3*v]

B2  = basis(A2)

A3 = [ A  A2( : , 1:3) ]

B3  = basis(A3)
% press any key
pause
clc
% Question 1: Given n  vectors, how would you decide if they are independent?
%  
% Question 2: How can you tell if a vector b is a combination of those vectors?
%  
% Question 3: Given TWO sets of vectors, how can you tell if they span the same
% subspace? Each vector must be a combination of the vectors in the OTHER SET.
%   
% Idea: Put the first vectors in the columns of A and put both sets in the
% columns of C. Look at basis(A) and basis(C). Does this give a complete answer?
%   
% Question 4: For a 3 by 4 matrix A what is the important fact about basis(A)
%   and basis(A')? 
%  
% press any key to see bases for the column space and row space
pause

A = [0 1 2 3; 4 5 6 7; 8 9 10 11];
colbasis = basis(A)
rowbasis = basis(A')

% press any key
pause
clc
% What are the important facts connecting the bases for the four subspaces?

nullbasis = null(A)
leftnull = null(A')
rowbasis'*nullbasis
colbasis'*leftnull
echo off
