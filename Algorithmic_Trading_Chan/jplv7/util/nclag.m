function xlag = nclag(x,lag1,n)
% PURPOSE: Generates a matrix of lags from a matrix containing
%          a set of vectors, x
%---------------------------------------------------
% USAGE: xlag = nclag(x,lag1,n)
% WHERE:    x    = an (nobs x neqs) matrix 
%           lag1 = beginning lag
%           n    = ending lag
%---------------------------------------------------
% e.g., xlag = nclag(x,2,3) would produce a matrix
%       [x(t-2) x(t-3)]
%---------------------------------------------------


[nobs, nvar] = size(x);
nlag = n - lag1 + 1;
xlag = zeros(nobs,nvar*nlag);
icnt = 1;
for i=1:nvar;
 for j=lag1:n;
 xlag(j+1:nobs,icnt) = x(1:nobs-j,i);
 icnt = icnt+1;
 end;
end;
