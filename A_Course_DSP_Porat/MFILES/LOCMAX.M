function [y,ind] = locmax(x);
% Synopsis: [y,ind] = locmax(x).
% Finds all local maxima in a vector and their locations,
% sorted in decreasing maximum values.
% Input:
% x: the input vector.
% Output parameters:
% y: the vector of local maxima values
% ind: the corresponding vector of indices of the input vector x.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

n = length(x); x = reshape(x,1,n);
xd = x(2:n)-x(1:n-1);
i = find(xd(1:n-2) > 0.0 & xd(2:n-1) < 0.0) + 1;
if (x(1) > x(2)), i = [1,i]; end
if (x(n) > x(n-1)), i = [i,n]; end
[y,ind] = sort(x(i)); ind = fliplr(ind);
ind = i(ind); y = x(ind);

