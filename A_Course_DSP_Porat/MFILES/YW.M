function [a,gammav] = yw(kappa);
% Synopsis: [a,gammav] = yw(kappa).
% Solves the Yule-Walker equations.
% Input:
% kappa: the covariance sequence value from 0 to p.
% Output parameters:
% a: the AR polynomial, with leading entry 1.
% gammav: the innovation variance

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

p = length(kappa)-1;
kappa = reshape(kappa,p+1,1);
a = [1; -toeplitz(kappa(1:p,1))\kappa(2:p+1,1)]';
gammav = a*kappa;

