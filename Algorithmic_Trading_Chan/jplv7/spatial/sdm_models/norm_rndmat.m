
function y = norm_rndmat(sig,ndraw)
% PURPOSE: random multivariate random vector based on
%          var-cov matrix sig
%---------------------------------------------------
% USAGE:   y = norm_rnd(sig)
% where:   sig = a square-symmetric covariance matrix 
% NOTE: for mean b, var-cov sig use: b +  norm_rnd(sig) 
%---------------------------------------------------      
% RETURNS: y = random vector normal draw mean 0, var-cov(sig)
%---------------------------------------------------

% by 
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com

if nargin ~= 2
error('Wrong # of arguments to norm_rnd');
end;

h = chol(sig);
[nrow, ncol] = size(sig);
rv = randn(nrow,ndraw);

y = h'*rv;


