function result = normrt_inv(p,mu,sigma2,right)
% PURPOSE: compute inverse of cdf for right-truncated normal
%          distribution, p = prob(Y<y | mu,sigma2)
%          where: y is a realization of r.v. Y
% ------------------------------------------------------
% USAGE: y = normrt_inv(p,mu,sigma2,right)
% where: p = probability (scalar or vector)
%       mu = mean (scalar or vector)
%   sigma2 = variance (scalar or vector)
%    right = right truncation point (scalar)
% ------------------------------------------------------
% RETURNS: y = invp (a scalar or vector)
% ------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
    
if nargin ~= 4
error('normrt_inv: Wrong # of arguments');
end;


  crgt=norm_cdf(right,mu,sigma2);
  if length(p) == 1
  res=norm_inv(p*crgt,mu,sigma2);
  else
  res = norm_inv(p.*crgt,mu,sigma2);
  end;
  tR= (res > right);
  ok= (res <= right);
  res=res.*ok + right*tR;
result = res;
