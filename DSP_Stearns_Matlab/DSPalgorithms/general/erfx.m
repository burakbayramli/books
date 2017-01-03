function y=erfx(mu,sigma,x)
% y=erfx(mu,sigma,x)
%
% erfx is the probability that a normally distributed random
% variate with mean mu and standard deviation sigma lies in
% the range {mu,mu+x}.
% See also: erf
v=x/(sqrt(2)*sigma);
y=.5*erf(v);
