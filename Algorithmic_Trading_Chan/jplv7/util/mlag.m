function xlag = mlag(x,n,init)
% PURPOSE: generates a matrix of n lags from a matrix (or vector)
%          containing a set of vectors (For use in var routines)
%---------------------------------------------------
% USAGE:     xlag = mlag(x,nlag)
%       or: xlag1 = mlag(x), which defaults to 1-lag
% where: x = a matrix (or vector), nobs x nvar
%     nlag = # of contiguous lags for each vector in x
%     init = (optional) scalar value to feed initial missing values
%            (default = 0)
%---------------------------------------------------
% RETURNS:
%        xlag = a matrix of lags (nobs x nvar*nlag)
%        x1(t-1), x1(t-2), ... x1(t-nlag), x2(t-1), ... x2(t-nlag) ...
% --------------------------------------------------
% SEE ALSO: lag() which works more conventionally
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin ==1 
n = 1; % default value
init = 0;
elseif nargin == 2
init = 0;
end;

if nargin > 3
error('mlag: Wrong # of input arguments');
end;

[nobs, nvar] = size(x);

xlag = ones(nobs,nvar*n)*init;
icnt = 0;
for i=1:nvar;
for j=1:n;
xlag(j+1:nobs,icnt+j) = x(1:nobs-j,i);
end;
icnt = icnt+n;
end;
