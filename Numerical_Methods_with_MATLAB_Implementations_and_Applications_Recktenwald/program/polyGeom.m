function [a,p] = polyGeom(s,n)
% polyGeom  Compute area and perimeter of a regular polygon
%
% Synopsis:  [a,p] = polyGeom(s,n)
%
% Input:  s = length of one side of the polygon
%         n = number of sides of the polygon
%
% Output: a = total area of the polygon
%         p = total perimeter of the polygon

r = s/(2*tan(pi/n));   %  "radius" of the polygon
a = area(r,n);
p = perimeter(r,n);

% ============ subfunction "area"
function a = area(r,n)
% area  Compute area of an n-sided polygon of radius r
a = n*r^2*sin(pi/n);

% ============ subfunction "perimeter"
function p = perimeter(r,n)
% perimeter  Compute perimeter of an n-sided polygon of radius r
p = n*2*r*tan(pi/n);

