function y = smartmean(x,dim)
%SMARTMEAN   Average or mean value ignoring NaN.
%
%   Same as MEAN except that it returns the mean of the finite elements
%   instead of propagating NaN and Inf
%   Returns NaN if there are no finite elements
%
%   For vectors, MEAN(X) is the mean value of the elements in X. For
%   matrices, MEAN(X) is a row vector containing the mean value of
%   each column.  For N-D arrays, MEAN(X) is the mean value of the
%   elements along the first non-singleton dimension of X.
%
%   MEAN(X,DIM) takes the mean along the dimension DIM of X. 
%
%   Example: If X = [0 1 2
%                    3 4 5]
%
%   then mean(X,1) is [1.5 2.5 3.5] and mean(X,2) is [1
%                                                     4]
%
%   See also MEDIAN, STD, MIN, MAX, COV.

%   Copyright (c) 1984-98 by The MathWorks, Inc.
%   $Revision: 5.13 $  $Date: 1997/11/21 23:23:55 $

if nargin==1, 
  % Determine which dimension SUM will use
  dim = min(find(size(x)~=1));
  if isempty(dim), dim = 1; end
  k=isfinite(x);
  x(~k)=0;
  warning off
  y=sum(x)./sum(k,dim);
  y(all(~isfinite(x)))=NaN;
  warning on
else
  k=isfinite(x);
  x(~k)=0;
  warning off
  y=sum(x,dim)./sum(k,dim);
  y(all(~isfinite(x), dim))=NaN;
  warning on
end
