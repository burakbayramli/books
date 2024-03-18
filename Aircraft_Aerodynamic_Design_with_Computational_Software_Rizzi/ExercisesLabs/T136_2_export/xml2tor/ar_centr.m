function [a,xc,yc]=ar_centr(x,y)
% Tornado function: compute area a and centroid (xc,yc) of closed polygon (x,y)
% Tornado function (internal): add info to span table of properties
% Input
%   tab   table
%   xint  what
%   pccon lin interp. or piecewise constant
%
% Output
%   tab   table for partitions
%
% calls
% --
%--------------------------------------------------------------------------
%  Revisions: KTH 091022 v 0
%
n = length(x);
a  = 0.5*sum(x(1:n-1).*y(2:n)-y(1:n-1).*x(2:n));
xc = sum(x(1:n-1)+x(2:n))/3;
yc = sum(y(1:n-1)+y(2:n))/3;
end