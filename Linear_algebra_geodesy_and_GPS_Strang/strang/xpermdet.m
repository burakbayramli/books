echo on
clc
% The determinant of a permutation matrix is always 1 or -1. This is also
% called the SIGN OF THE PERMUTATION. It is +1 if the number of exchanges 
% to reach P from the identity (or to get back to the identity) is EVEN.
%  
% Our examples and questions are about
% 1) The signs of some special permutations
% 2) The sign of P squared
% 3) The sign of P inverse
%  
% Our special P's will be the REVERSE IDENTITY MATRICES. Call them J.

J3 = [0 0 1; 0 1 0; 1 0 0]
D3 = det(J3)
J4 = [0 0 0 1; 0 0 1 0; 0 1 0 0; 1 0 0 0 ]
D4 = det(J4)
% press any key
pause
clc
% What is your prediction for the determinant D5 of the 5 by 5 reverse
% identity J5?  MATLAB has better ways to create J5 than to type 25 entries.

%  J5 = fliplr(eye(5,5))
%    or
%  J5 = rot90(eye(5,5),2)
%    or
I = eye(5,5);
k = 5:-1:1;
J5 = I(:,k);
D5 = determ(J5)

% Now choose P at random, square it, invert it, transpose it:

p = randperm(4)
I4 = eye(4);
P = I4( p , :)
SQ = P*P

%press any key
pause
clc
INV = inverse(P)
% Find the determinants of P and P*P and inverse(P). What is the rule?

D = det(P)
DSQ = det(SQ)
DINV = det(INV)
echo off
