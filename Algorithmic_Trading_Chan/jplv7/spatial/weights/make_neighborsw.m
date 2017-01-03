function W = make_neighborsw(xc,yc,m)
% PURPOSE: constructs a row-stochastic nearest neighbor spatial weight matrix
%          asymmetric, but row-sums are unity, based on m neighbors
% --------------------------------------------------------
% USAGE: W = make_neighborsw(xc,yc,nn)
%       where: 
%             xc = x-coordinate for each obs (nobs x 1)
%             yc = y-coordinate for each obs (nobs x 1)
%             nn = # of nearest neighbors to be used
% --------------------------------------------------------
% RETURNS: W an (nobs x nobs) spatial weight matrix based on nn
%          nearest neighbors (a sparse matrix)
% --------------------------------------------------------
% NOTES: 
%        W takes a form such that: W*y would produce a vector
%        consisting of the values of y for the nn nearest neighbors
%        for each observation i in the (nobs x 1) vector y
% To construct a weight matrix based on 4 nearest neighbors
% W4 = make_neighborsw(xc,yc,4);
%   ---> This function will is similar to make_nnw, but uses less
%        memory and takes more time. If you run out of memory using
%        make_nnw, try this function
% --------------------------------------------------------
% SEE ALSO: find_neighbors(), find_nn(), make_nnw()
% --------------------------------------------------------

% written by:
% James P. LeSage, 5/2002
% updated 1/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


if nargin == 3
[n junk] = size(xc);    
else,
error('make_neighborsw: Wrong # of input arguments');
end;


nnlist = find_neighbors(xc,yc,m);

% convert the list into a row-standardized spatial weight matrix
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
z=[z1
   z2];

if i == 1
    W = spconvert(z);
else
    W = W + spconvert(z);
end;

end;


