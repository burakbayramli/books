echo on
clc
% Start with the determinant of a pascal matrix that has the pascal
% triangle in its entries. Then we change it a little and look at determ 

A = pascal(5)

determ(A)
% Every pascal matrix has determ = 1 
% Let us subtract 1 from the 70 in the corner:
%  

v = [0 0 0 0 1];
B = A - v'*v


determ(B)

% Why did the det change to zero ??  The cofactor of that (5,5) entry is 1  
% To find that cofactor use cofactor(A) or cofactor (A,5,5).  
% Or you could just find determ(pascal(4)) which equals the cofactor.
% press any key
pause
clc 
% If you change only one entry of a matrix, what is the effect on the det?
% Suppose you subtract 2 instead of 1 from the 70 in pascal(5).
% What determinant do you predict?   Test:

C = A - 2*v'*v

determ(C)
%   
% press any button
pause
% To professors: 
% What is the det after you subtract 1 from the (1,1) entry of pascal(5)?
% (Not the (5,5) entry this time) For this you will need the cofactor matrix 

cof = cofactor(A)

w = [1 0 0 0 0];
D = pascal(5) - w'*w
determ(D)
echo off






