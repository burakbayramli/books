function nnlist = find_nn(xc,yc,m,order)
% PURPOSE: finds observations containing m nearest neighbors, fast but high memory version
%          to each observation and returns an index to these neighboring observations
% --------------------------------------------------------
% USAGE: nnindex = find_nn(xc,yc,m)
%       where: 
%             xc = x-coordinate for each obs (nobs x 1)
%             yc = y-coordinate for each obs (nobs x 1)
%             m  = # of nearest neighbors to be found
%          order = 1, 2, 3, or 4. (default = 2)
%                  This limits the extent to search for neighbors to the sum of the delaunay orders
%                  e.g., delorder=2 limits the search to the delaunay neighbors and the neighbors of neighbors
%                  Lower values of delorder speed computation and reduce memory. 
% --------------------------------------------------------
% RETURNS: an (nobs x m) matrix of indices to the m neighbors
% --------------------------------------------------------
% NOTES: nnindex takes a form such that: ind = nnindex(i,:)';
%        y(ind,1)
%        would pull out the nn nearest neighbor observations to
%        y(i,1), and y(ind,1)/m would represent an avg of these
%        This function is a fneighbors2 from R. Kelley Pace's spatial
%        statistics toolbox 
% --------------------------------------------------------
% SEE ALSO: find_neighbors, make_nnw which constructs a standardized spatial 
%           weight matrix based on these Delaunay nearest neighbors
% --------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% NOTE: this function draws on ideas from the Spatial Statistics Toolbox
%       by R. Kelley Pace (that I stole to construct this function)


if nargin == 3
order = 2;
[W1,W2,W3] = xy2cont(xc,yc);
nnlist = fneighbors2(W2,xc,yc,m,order);
elseif nargin == 4
[W1,W2,W3] = xy2cont(xc,yc);
nnlist = fneighbors2(W2,xc,yc,m,order);
else
error('find_nn: Wrong # of input arguments');
end;
