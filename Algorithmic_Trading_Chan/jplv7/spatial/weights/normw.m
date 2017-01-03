function W = normw(W)
% PURPOSE: normalize a spatial weight matrix
%          to have row sums of unity
% -------------------------------------------------
% USAGE: Wout = normw(W)
% where: W  = (n x n) input weight matrix
% -------------------------------------------------
% NOTES: i) some rows can be zero, 
%        2) the function works for 3-d matrices 
%           If W is a 3D array, sum_k W(i,j,k)=1 for all i,j.
% ------------------------------------------------- 
% RETURNS: wout = standardized weight matrix
% -------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


if nargin ~= 1
    error('normw: Wrong # of input arguments');
end;

 [n1 n2] = size(W);
if n1 ~= n2,
 error('normw: W matrix must be square');
end;

  n = ndims(W);
  nterm = sum(W, n);
  nterm = repmat(nterm,[ones(1,n-1) size(W,n)]);
  % protect against zeros before dividing
  nterm = nterm + (nterm==0);
% use the same W as output here to save RAM
  W = W ./ nterm;

