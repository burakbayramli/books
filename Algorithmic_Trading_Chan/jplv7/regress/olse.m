function resid=olse(y,x)
% PURPOSE: OLS regression returning only residual vector
%---------------------------------------------------
% USAGE: residual = olse(y,x)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%---------------------------------------------------
% RETURNS: the residual vector
%---------------------------------------------------
% NOTE: used by sur() function
% --------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if (nargin ~= 2); error('Wrong # of arguments to olse'); end;

beta = (x'*x)\(x'*y);
resid = y - x*beta;

