function [r,xmin,xmax]=range(x)
% [r,xmin,xmax]=range(x)
%
% r=range(x)=max(x)-min(x)
% xmin=min(x)
% xmax=max(x)
%
% Works on vectors and arrays same as functions min and max.
% If x is an array, you get a range on each column.

xmin=min(x);
xmax=max(x);
r=xmax-xmin;