echo on
clg
clc
% The linefit program draws the closest (least squares) line to a set of points
% in the plane. Start with random points in a square, at times t and heights b.
% The least squares solution x gives the coefficients in the best line.
t = rand(5,1);
b = rand(5,1);
c = [1 1 1 1 1]';
A = [c t]
linefit(t,b)
pause(2)
x = lsq(A,b)
e = A*x - b
E = e'*e
% press any key for a discussion of E
pause
clc
% The distances to that best line are in e. Those are VERTICAL distances. 
% The sum of squared errors is E = squared distance from b to plane of c and t.

% Any other line (any other x) gives a larger E -- this is least squares!   
% I have no idea what is the average value of E from random vectors t and b.

% With a fixed vector of times t = [0 .2 .4 .6 .8]' and random heights b,
% this a challenge project. By experiment find the average value of E.
%  
% Here are special linefits for special b's. Explain the results.
b = c;
% press any key to draw the best line when b = c = [1 1 1 1 1]'
pause
linefit(t,b)
pause(2)
b = t;
% press any key to draw the best line when b = t
pause
linefit(t,b)
pause(2)
b = e;
% press any key to draw the best line when b = e
pause
linefit(t,b)
echo off
