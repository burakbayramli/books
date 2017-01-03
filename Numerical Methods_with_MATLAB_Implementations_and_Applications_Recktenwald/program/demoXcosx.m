function s = demoXcosx(n)
% demoXcosx  Use an inline function object with the fsum function
%
% Synopsis:  s = demoXcosx
%
% Input:     n = number of points at which x*cos(x) is evaluated
%                in the interval 0 <= x <= 2*pi
%
% Output:    s = sum of x*cos(x) at n points

xcosx = inline('x.*cos(x)');
s = fsum(xcosx,0,3*pi,n);
