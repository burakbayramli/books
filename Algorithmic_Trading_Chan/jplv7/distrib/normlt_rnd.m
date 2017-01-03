function result = normlt_rnd(mu,sigma2,left)
% PURPOSE: compute random draws from a left-truncated normal
%          distribution, with mean = mu, variance = sigma2
% ------------------------------------------------------
% USAGE: y = normlt_rnd(mu,sigma2,left)
% where:   mu = mean (scalar or vector)
%      sigma2 = variance (scalar or vector)
%        left = left truncation point (scalar or vector)
% ------------------------------------------------------
% RETURNS: y = (scalar or vector) the size of mu, sigma2
% ------------------------------------------------------
% NOTES: This is merely a convenience function that
%        calls normt_rnd with the appropriate arguments
% ------------------------------------------------------

% written by:
% James P. LeSage, Dept of Finance & Economics
% Texas State Univeristy-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com
% Last updated 10/2007

if nargin ~= 3
error('normlt_rnd: Wrong # of input arguments');
end;

nobs = length(mu);
right = 999*ones(nobs,1);

result = normt_rnd(mu,sigma2,left,right);

