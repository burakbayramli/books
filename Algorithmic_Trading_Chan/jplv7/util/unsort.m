function out = unsort(xsorted,xindex)
% PURPOSE: takes a sorted vector (or matrix) and sort index as input
%         and returns the vector (or matrix) in original unsorted form
% --------------------------------------------------
% USAGE: x = unsort(xsorted,xindex)
% where: xsorted = a vector created with:
%                  [xsorted xindex] = sort(x);
%         xindex = the vector returned from sort()
% NOTE: xindex can't be a matrix
% -------------------------------------------------
% RETURNS: x that was input to the sort() function
% --------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


if nargin == 2
[n k] = size(xsorted);
[nchk kchk] = size(xindex);
 if nchk ~= n
 error('unsort: inputs are different size');
 elseif kchk ~= 1
 error('unsort: index must be a vector');
 end;

out = zeros(n,k);
for i=1:n
out(xindex(i,1),:) = xsorted(i,:);
end;

else
error('unsort: Wrong # of input arguments');
end;
