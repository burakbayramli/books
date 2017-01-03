function W = make_nnw(xc,yc,m,order)
% PURPOSE: finds the n nearest neighbors and constructs a spatial weight matrix 
%          based on this # of neighbors, normalized to have row-sums of unity
% --------------------------------------------------------
% USAGE: W = make_nnw(xc,yc,nn)
%       where: 
%             xc = x-coordinate for each obs (nobs x 1)
%             yc = y-coordinate for each obs (nobs x 1)
%             nn = nth nearest neighbor to be used
%          order = 1, 2, 3, or 4. (default = 2)
%                  This limits the extent to search for neighbors to the sum of the delaunay orders
%                  e.g., delorder=2 limits the search to the delaunay neighbors and the neighbors of neighbors
%                  Lower values of delorder speed computation and reduce memory. 
% --------------------------------------------------------
% RETURNS: W = (nobs x nobs) spatial weight matrix based on the nn
%          nearest neighbors (a sparse matrix), row-sums of unity
% --------------------------------------------------------
% NOTES: This function embodies fneighbors2 
%        from Spatial Statistics Toolbox by R. Kelley Pace
% --------------------------------------------------------
% SEE ALSO: find_nn(), make_xyw() 
% --------------------------------------------------------

% written by:
% James P. LeSage, 1/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% NOTE: this function draws on ideas from the Spatial Statistics Toolbox 2
%       by R. Kelley Pace (that I stole to construct this function)

if nargin == 3
order = 2;
elseif nargin == 4
% we use the order input by the user
else
error('make_nnw: 3 or 4 input arguments required');
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
error('make_nnw: xc,yc inputs must be same size');
end;

nnlist = find_nn(xc,yc,m,order);

rowseqs=(1:n)';
vals1=ones(n,1);
vals0=zeros(n,1);


%this next set of statements converts the indices into a
% row-stochastic weight matrix

rowseqs=(1:n)';
vals1=ones(n,1)*(1/m);
vals0=zeros(n,1);

for i=1:m;

colseqs=nnlist(:,i);
ind_to_keep=logical(colseqs>0);

z1=[rowseqs colseqs vals1];
z1=z1(ind_to_keep,:);

z2=[rowseqs rowseqs vals0];
%this last statement makes sure the dimensions are right
z=[z1;z2];

if i == 1
    W = spconvert(z);
else
    W = W + spconvert(z);
end;


end;


