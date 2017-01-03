function resid = var_resid(y,nlag,x)
% PURPOSE: performs vector autogressive estimation
%          and returns only residuals
%          (used by lrratio.m)
%---------------------------------------------------
% USAGE: resid = var_resid(y,nlag,x) 
% where:    y    = an (nobs x neqs) matrix of y-vectors
%           nlag = the lag length
%           x    = optional matrix of variables (nobs x nx)
%                 (NOTE: constant vector automatically included)
%---------------------------------------------------
% RETURNS: a matrix of residuals (nobs x neqs)
%---------------------------------------------------
% SEE ALSO: var, varf, prt_var 
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs neqs] = size(y);

nx = 0;

if nargin == 3
[nobs2 nx] = size(x);
 if (nobs2 ~= nobs)
 error('var_resid: nobs in x-matrix not the same as y-matrix');
 end;
end;

% adjust nobs to feed the lags
nobse = nobs - nlag;

% nvar adjusted for constant term 
 k = neqs*nlag + 1 + nx;
 nvar = k;

xlag = mlag(y,nlag);

% form x-matrix
if nx 
xmat = [xlag(nlag+1:nobs,:) x(nlag+1:nobs,:) ones(nobs-nlag,1)];
else
xmat = [xlag(nlag+1:nobs,:) ones(nobs-nlag,1)];
end;

resid = zeros(nobse,neqs);

% pull out each y-vector and run regressions
for j=1:neqs;
 yvec = y(nlag+1:nobs,j);
 resid(:,j) = olse(yvec,xmat);
end; 
% end of loop over equations 
 



