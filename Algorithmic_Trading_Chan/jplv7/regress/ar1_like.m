function llike = ar1_like(param,y,x)
% PURPOSE: evaluate ols model with AR1 errors log-likelihood
%-----------------------------------------------------
% USAGE:    like = ar1_like(b,y,x) 
% where:     b = parameter vector (k x 1)
%            y = dependent variable vector (n x 1)
%            x = explanatory variables matrix (n x m)
%-----------------------------------------------------
% NOTE: this function returns a scalar equal to -log(likelihood)
%       b(1,1) contains rho parameter
%       sige is concentrated out
%-----------------------------------------------------
% REFERENCES: Green, 1997 page 600
%-----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[n k] = size(x);
rho = param(1,1); beta = param(2:2+k-1,1);
ys = y - rho*lag(y);
xs = x - rho*lag(x);
ys(1,1) = sqrt(1-rho*rho)*y(1,1);
xs(1,:) = sqrt(1-rho*rho)*x(1,:);
term1 = -(n/2)*log((ys - xs*beta)'*(ys - xs*beta));
term2 = 0.5*log(1-rho*rho);
like = term1+term2;
llike = -like;


