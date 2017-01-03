function [beta] = logregFit(y,X)
% logregFit.m MLE for logistic regression
% Based on code by David Martin 
%
% Rows of X contain data
% y(i) = 0 or 1
%
% Returns beta, a row vector

[N,p] = size(X);
beta = zeros(p,1); % initial guess for beta: all zeros
iter = 0;
tol = 1e-4; % termination criterion based on loglik
nll = 0;
while 1
  iter = iter + 1;
  nll_prev = nll;
  [nll, g, H] = logregGradient(beta, X,y);
  g = -g; % logregGradient returns gradient of negative log likelihood
  beta = beta - H\g; % more stable than beta - inv(hess)*deriv
  if abs((nll-nll_prev)/(nll+nll_prev)) < tol, break; end;
end;
