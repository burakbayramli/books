function [a,t] = lor(Y)
%LOR	 Linear orthogonal regression.
%	    See Dahlquist & Bj›rck, 2nd edition, (1995) Example 7.6.2
%	    The first column of Y contains y and the second column x
%	    for p given points

%Kai Borre 03-27-94
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

[p,q] = size(Y);
e = ones(p,1);
m = mean(Y);
Y = Y-e*m;
[U,S,V] = svd(Y);
cs = V(:,1);
alpha = atan2(cs(2),cs(1));
t = tan(alpha);
%Following statement is necessary due to special orientation
%of coordinate system
x = tan(-alpha+pi/2); 
a = m(2)-1/x;
%%%%%%%% end lor.m  %%%%%%%%%%%%%
