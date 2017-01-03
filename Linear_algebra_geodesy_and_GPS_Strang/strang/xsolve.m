echo on
clc

% Choose a square matrix with dependent columns u, v, w.

u = [1 2 1]';  v = [1 1 1]';
w = 2*u + 4*v;
A = [u  v  w]
xparticular = solve(A,w)
% Choose another right side b that is in the column space of A.

b = 3*u + 5*v;

% Find a particular solution to Ax = b.

xparticular = solve(A,b)
% Question:  What is the complete solution to Ax = b?

% Question:  What are the particular solutions solve(B,b) and solve(C,b)
%            when the columns are reordered to give the matrices
%            B = [v u w] and C = [u w v]?
%  
% press any key
pause
clc

% Question:  For the original b = 3u + 5v what will be the particular
%            solution for the matrices M = [u u v] and S = [u u+v]?
%  
% Final experiment:  Change b to a random right hand side randb.
%  Solve again and explain why it fails:
%  
randomb = rand(3,1)
xparticular = solve(A,randomb)
echo off
