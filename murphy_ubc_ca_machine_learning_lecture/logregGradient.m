function [f,g,H] = logregGradient(beta, X, y, lambda)
% logregGradient - gradient and hessian of *negative* loglik for logistic regression
%
% Rows of X contain data
% y(i) = 0 or 1

if nargin < 4,lamdba = 0; end
mu = 1 ./ (1 + exp(-X*beta)); % mu(i) = prob(y(i)=1|X(i,:))
f = -sum( (y.*log(mu+eps) + (1-y).*log(1-mu+eps))) + lambda/2*sum(beta.^2); 
g = -X'*(y-mu) + lambda*beta;
W = diag(mu .* (1-mu)); %  weight matrix
H = X'*W*X + lambda*eye(length(beta));
