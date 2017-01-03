function kappa = kappahat(x,p);
% Synopsis: kappa = kappahat(x,p).
% Generate estimated covariance values of a data sequence.
% Input parameters:
% x: the data vector
% p: maximum order of covariance.
% Output parameters:
% kappa: the vector of kappahat from 0 through p.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

x = x - mean(x); N = length(x);
kappa = sum(x.*x);
for i = 1:p,
   kappa = [kappa, sum(x(1:N-i).*x(i+1:N))];
end
kappa = (1/N)*kappa;

