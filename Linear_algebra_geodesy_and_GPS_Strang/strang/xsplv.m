echo on
clc
%  
% The code splv solves Ax = b when A is square and invertible.
%  
% Example 1: Suppose b is the sum of the columns of a random matrix M.
% Explain why the solution to Mx = b is 1,1,1:

M = round(10*rand(3));
b = sum(M')';

x = splv(M,b)
% Example 2: The matrix A = ones - eye has zeros along its main diagonal.
% Is it still invertible? If not, the code will fail. Certainly slu and slv 
% will fail because A (1,1) = 0 and a row exchange is required. Experiment:

b = [1 0 0 0]'; A = ones(4) - eye(4)
x = splv(A,b)
% press any key
pause
clc
%  
% Question 1:  To solve A*B*x = b we do NOT want to multiply A*B. This is too
% expensive. Which of these programs will give the correct x?
%  
%                      y = splv(A,b); x = splv(B,y);
%                                   OR
%                      y = splv(B,b); x = splv(A,y);
%  
% Question 2: What is the chance that a random permutation P is invertible?
% What is the chance that Px = b can be solved without any row exchanges?
% What is the chance that P(1,1) = 1? The permutation P is 4 by 4.
%  
% Question 3:  A contains nine 0's and 1's at random: 2^9 = 512 possibilities  
% I can't predict if splv or slv will work or fail. Which happens more often?
%
% Repeat the program (up arrow then return) until you get an invertible A.
% The code zeroone does this experiment in a good way. Try it next.
b = [1 1 1]'; A = round(rand(3))
x = splv(A,b)
x = slv(A,b)
echo off



