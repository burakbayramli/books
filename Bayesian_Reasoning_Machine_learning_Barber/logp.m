function l=logp(x)
%LOGP The logarithm of a specific non-Gaussian distribution.
% See also demoMetropolis.m
l1 = exp(- (x(1)^2+x(2)^2+sin(x(1)+x(2))^2));
f=3; % larger f makes the distribution more bimodal.
l2 = exp(- ((x(1)-f)^2+(x(2)-f)^2+sin(x(1)+x(2)-2*f)^2));
l=log(l1+l2);