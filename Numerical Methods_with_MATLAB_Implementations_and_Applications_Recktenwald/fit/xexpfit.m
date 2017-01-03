function c = xexpfit(x,y)
% xexpfit    Least squares fit of data to y = c(1)*x*exp(c(2)*x)
%
% Synopsis:  c = xexpfit(x,y)
%
% Input:     x,y = vectors of independent and dependent variable values
%
% Output:    c = vector of coefficients of  y = c(1)*x*exp(c(2)*x)

z = log(y./x);             %  Natural log of element-by-element division
c = linefit(x,z);          %  Fit is performed by linefit
c = [exp(c(2)); c(1); ];   %  Extract parameters from transformation
