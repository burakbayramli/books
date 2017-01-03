function [a,rho,s] = levdur(kappa);
% Synopsis: [a,rho,s] = levdur(kappa).
% The Levinson-Durbin algorithm.
% Input:
% kappa: the covariance sequence values from 0 to p.
% Output parameters:
% a: the AR polynomial, with leading entry 1
% rho: the set of p reflection coefficients
% gammav: the innovation variance.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

p = length(kappa)-1;
kappa = reshape(kappa,p+1,1);
a = 1; s = kappa(1); rho = [];
for i = 1:p,
   rhoi = (a*kappa(i+1:-1:2))/s; rho = [rho,rhoi];
   s = s*(1-rhoi^2);
   a = [a,0]; a = a - rhoi*fliplr(a);
end

