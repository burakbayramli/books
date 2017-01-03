echo on
clc 
%  
% For every matrix A, the reduced echelon form R has all pivots = 1 and zeros
% above and below the pivots. The row operations that change A into R could be
% put in a square matrix M, so that MA = R.
%  
% An important application is to find the inverse of A. If A is invertible
% then R = I. In that case M is the inverse matrix. Gauss-Jordan elimination
% finds this inverse by starting with the block matrix [A I].
%  
% Then ref multiplies [A I] by M to give [I M]. The matrix M = inverse of A is
% read off by the code called inverse. Here are examples A1 and A2:

A1 = [3 7; 2 5]
R1 = ref(A1)
GAUSSJ1 = ref([A1 eye(2)])
INV1 = GAUSSJ1(: , 3:4)
% press any key
pause
clc

A2 = [2 4; 3 6]
R2 = ref(A2)
% This is not I. Therefore A2 is not invertible. The code null uses ref
% to find the nullspace matrix, which was empty for the first matrix A1:

N2 = null(A2)
% Question 1: If R is the reduced echelon form of A, is it true that [R R]
% is the reduced echelon form of [A A]?
%   
% Question 2: How can you find the rank of A from its reduced echelon form R?
%  
% Question 3: How can you find a basis for the row space of A from R?

echo off
