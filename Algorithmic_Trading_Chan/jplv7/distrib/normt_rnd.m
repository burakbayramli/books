function result = normt_rnd(mu,sigma2,left,right)
% PURPOSE: random draws from a normal truncated to (left,right) interval
% ------------------------------------------------------
% USAGE: y = normt_rnd(mu,sigma2,left,right)
% where:   mu = mean (nobs x 1)
%      sigma2 = variance (nobs x 1)
%        left = left truncation points (nobs x 1),   999 for +Infinity
%       right = right truncation points (nobs x 1), -999 for -Infinity
% ------------------------------------------------------
% RETURNS: y = (nobs x 1) vector
% ------------------------------------------------------
% NOTES: use y = normt_rnd(mu,sigma2,left,999)
%        to produce a left-truncated draw
%        use y = normt_rnd(mu_vector,sigma2_vector,-999*ones(nobs,1),right_vector)
%        to produce a vector of right-truncated draws
% ------------------------------------------------------
% SEE ALSO: normlt_rnd (left truncated draws), normrt_rnd (right truncated)
%

% adopted from truncnorm3.m file by
% Justin Tobias
% James P. LeSage, Dept of Finance & Economics
% Texas State Univeristy-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com
% last updated 10/2007
%
% For information on Bayesian Econometric Methods: 
% Volume 7 of Econometric Exercises Series
% Cambridge University Press by Gary Koop, Dale Poirer and Justin Tobias
% see:
% www.econ.iastate.edu/faculty/tobias/Bayesian_exercises.html


if nargin ~= 4
error('normt_rnd: wrong # of input arguments');
end;

stderrs = sqrt(sigma2);

points_left = find(left==-999);
points_right = find(right==999);

a_term = norm_cdf( (left-mu)./stderrs);
a_term(points_left) = 0;

b_term = norm_cdf( (right-mu)./stderrs);
b_term(points_right) = 1;

uniforms = rand(length(mu),1);

p = a_term + uniforms.*(b_term - a_term);

result = mu + stderrs.*norm_inv(p);
