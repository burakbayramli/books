%IMPACT OF CORRELATION ON MEAN VALUE 
% Demonstration of the influence of correlation on
% the mean value of two numbers

%Kai Borre 29-10-94
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26 $

clc
A = ones(2,1);
b = [100;102];
axis([99.5 102.5 0 2])
echo on
% Before starting be sure that you are able to
% see what happens in both the workspace
% and the figure window!
%
%
% We start by demonstrating results from
% a diagonal covariance matrix
% First we use the identity matrix
echo off
C = eye(2);
x = lscov(A,b,inv(C));
plot([100 102], [1 1],'o', x,[1],'x')
pause
echo on
% Then we let the weight of the first
% observation tend to infinity
echo off
hold on
for t = 1:5
  	C(1,1) = 10^t;
  	x = lscov(A,b,inv(C));
   plot([100 102], [1 1],'o', x,[1],'x')
  	pause(2)
end
pause
hold off
C(1,1) = 1;
echo on
% Next we move in the opposite direction
% by letting the weight of the second
% observation tend to infinity
echo off
hold on
for t = 1:5
	C(2,2) = 10^t;
	x = lscov(A,b,inv(C));
	plot([100 102], [1 1],'o', x,[1],'x')
	pause(2)
end
pause
hold off
echo on
% Now we study covariance matrices including
% CORRELATION. In some cases this does not change
% things quantitatively. Let us try
% C = [1 .1; .1 3];
echo off
cla
C = [1 .1; .1 3];
x = lscov(A,b,inv(C));
plot([100 102], [1 1],'o', x,[1],'x')
pause
echo on
% But now we shall show something which for sure
% will surprise you: Some choices of C with
% non-zero off-diagonal components may result in
% solutions lying outside the interval between the
% two observations!
% Look here for example
% C = [1.010101 -10; -10 100];
echo off
C = [1.010101 -10; -10 100];
x = lscov(A,b,inv(C));
plot([100 102], [1 1],'o', x,[1],'x')
pause
% This phenomenon shall be investigated in
% Chapter 11
%%%%%%%%% end corrdemo.m  %%%%%%%%%%%%%%%%%%%
