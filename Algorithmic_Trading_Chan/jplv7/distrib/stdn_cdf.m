function cdf = stdn_cdf(x)  
% PURPOSE: computes the standard normal cumulative
%          distribution for each component of x
%---------------------------------------------------
% USAGE: cdf = stdn_cdf(x)
% where: x = variable vector (nx1)
%---------------------------------------------------
% RETURNS: cdf (nx1) vector
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

  if (nargin ~= 1)
    error('Wrong # of arguments to stdn_cdf');
  end;

  cdf = .5*(ones(size(x))+erf(x/sqrt(2)));


