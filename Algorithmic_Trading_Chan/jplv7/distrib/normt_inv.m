function result = normt_inv(p,mu,sigma2,left,right)
% PURPOSE: compute inverse of cdf for truncated normal
%          distribution, p = prob(Y<y | mu,sigma2)
%          where: y is a realization of r.v. Y
% ------------------------------------------------------
% USAGE: y = normt_inv(p,mu,sigma2,left,right)
% where: p = probability (scalar or vector)
%       mu = mean
%   sigma2 = variance
%     left = left truncation point
%    right = right truncation point
% ------------------------------------------------------
% RETURNS: y = invp (a scalar or vector)
% ------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
    
if nargin ~= 5
error('normt_inv: Wrong # of arguments');
end;

  clft=norm_cdf(left,mu,sigma2);
  crgt=norm_cdf(right,mu,sigma2);
  if length(p) == 1
   res=norm_inv(p*(crgt-clft)+clft,mu,sigma2);
  else
   res = norm_inv(p.*(crgt-clft)+clft,mu,sigma2);
  end;
  tL= (res < left);
  tR= (res > right);
  ok=(res >= left) & (res <= right);
  res=res.*ok+left*tL + right*tR;
result = res;
