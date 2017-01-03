echo on
clc

% The least squares solution x = lsq(A,b) is the exact solution of A'Ax = A'b.
% The matrix A'A is invertible if and only if A has independent columns. 

% Then the projection of b onto the column space of A is p = Ax.
% p = b when b is in the column space! Otherwise Ax = b has no exact solution.
%  
% The error e = Ax - b is perpendicular to the columns of A because A'e = 0.
%  
% Example: Find the best horizontal line y = C, the best sloping line y = C + Dt
% and the best parabola y = C + Dt + Et^2 to fit this b at the times t:
%  
t = [0 1 2 3]'; b = [3 5 8 8]'; one = [1 1 1 1]'; tt = [0 1 4 9]';

A = [one t]; parab = [one t tt]

bestC = lsq(one, b)
bestCD = lsq(A,b)
bestCDE = lsq(parab,b)
% press any key
pause
clc
% Question 1: If the times 0, 1, 2, 3, are shifted to 1, 2, 3, 4, explain how
%             this changes C and D for the best line. Here are the new C and D:

ANEW = [one   t + one]
CDNEW = lsq(ANEW, b)
% Question 2: Find the three error vectors eone, eline, eparab. Their lengths
%           should be decreasing. Why do the four error components add to zero?
%  
% Question 3: Compute the best cubic y =  C + Dt + Et^2 + Ft^3 by adding a new
%           column ttt = [0 1 8 27]. Find the error vector.
%   
% Example 2: Fit the same points by the best wave y = G + H cos t.
% MATLAB puts the cosines of the times t = [0 1 2 3]' into the vector cos (t).

ATRIG = [one cos(t)]
bestGH = lsq(ATRIG, b)
% Question 4: What single instruction will produce the projection p of 
%             the vector b onto the line through the vector a ?
echo off






