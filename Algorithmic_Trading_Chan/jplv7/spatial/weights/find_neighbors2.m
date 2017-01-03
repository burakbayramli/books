function nnlist = find_neighbors(xc,yc,m)
% PURPOSE: finds observations containing m nearest euclidean distance-based neighbors,
%          (slow but low memory version) returns an index to these neighboring observations
% --------------------------------------------------------
% USAGE: nnindex = find_neighbors(xc,yc,m)
%       where: 
%             xc = x-coordinate for each obs (nobs x 1)
%             yc = y-coordinate for each obs (nobs x 1)
%             m  = # of nearest neighbors to be found
% --------------------------------------------------------
% RETURNS: an (nobs x m) matrix of indices to the m neighbors
% --------------------------------------------------------
% NOTES: nnindex takes a form such that: ind = nnindex(i,:)';
%        y(ind,1) would pull out the m nearest neighbor observations to
%        y(i,1), and y(ind,1)/m would represent an avg of these
%   ---> This function will is similar to find_nn, but uses less
%        memory and takes more time. If you run out of memory using
%        find_nn, try this function
% --------------------------------------------------------
% SEE ALSO: find_nn, make_neighborsw, make_nnw, make_xyw
% --------------------------------------------------------

% written by:
% James P. LeSage, 12/2001
% modified 1/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% NOTE: this is a fast approach, but requires a lot of RAM memory

if nargin ~= 3
error('find_neighbors: 3 input arguments required');
end;

% error checking on inputs
[n junk] = size(xc);
if junk ~= 1
xc = xc';
end;
[n2 junk] = size(yc);
if junk ~= 1
yc = yc';
end;
if n ~= n2
error('find_neighbors: xc,yc inputs must be same size');
end;

nnlist = zeros(n,m);

dist = distance(xc,yc);
for i=1:n;
[xds xind] = sort(dist);
nnlist(i,1:m) = xind(2:m+1,1)';
end;




