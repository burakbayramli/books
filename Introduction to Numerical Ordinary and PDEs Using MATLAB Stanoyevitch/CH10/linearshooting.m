function [t, y] = linearshooting(pstring, qstring, rstring, a, alpha, b, beta, hstep)
%M-file for EFR 10.6
%This program will use the linear shooting method to solve a linear 
%BVP of the following form: y''(t)=p(t)y'+q(t)y+r(t), y(a)=alpha, y(b)=beta
%Input variables:  pstring = string for the function for p(t),
%qstring =string for the function for q(t), rstring = string for r(t), 
%a, alpha, b, beta are numbers as in the BVP, hstep is a postive 
%number to be used in the Runge-Kutta method. 
%Output variables: t and y, vectors of the same size that give the 
%time values and associated numerical solution values.
%NOTE:  The first three input variables must be put in single quotes (so MATLAB 
%will assign their data types to be strings).  Within the program, we will
%need to create inline functions in terms of the formulas for p(t), q(t),
%and r(t).  This would not be possible if instead we had these three
%functions inputted as inline functions.
%IMPORTANT:  the independent variable of the inputted strings for p, q and
%r must be t.  

%Step 1:  Set up the functions for the linear systems corresponding to the
%two associated IVP's and solve each one.
%IVP-1:  y1''(t)=p(t)y1'+q(t)y1+r(t), y1(a)=alpha, y1'(a)=0
y1p = inline('u', 't', 'y', 'u');
u1p = inline(['(', pstring, ')*u+(', qstring, ')*y+' rstring],'t','y','u');
[t,y1,u1]=runkut2d(y1p,u1p,a,b,alpha,0,hstep);
%IVP-2:  y2''(t)=p(t)y2'+q(t)y2, y2(a)=0, y2'(a)=1
y2p = inline('u', 't', 'y', 'u');
u2p = inline(['(', pstring, ')*u+', qstring, '*y'],'t','y','u');
[t,y2,u2]=runkut2d(y2p,u2p,a,b,0,1,hstep);

%Step 2:  Construct solution of BVP
y=y1+(beta-y1(find(t==b)))/y2(find(t==b))*y2;
