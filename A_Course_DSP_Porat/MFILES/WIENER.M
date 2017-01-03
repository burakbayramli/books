function b = wiener(kappax,kappayx);
% Synopsis: b = wiener(kappax,kappayx).
% Solves the Wiener equation.
% Input parameters:
% kappax: the covariance sequence of x from 0 to q
% kappayx: the joint covariance sequence of y and x from 0 to q.
% Output:
% b: the Wiener filter.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

q = length(kappax)-1;
kappax = reshape(kappax,q+1,1);
kappayx = reshape(kappayx,q+1,1);
b = (toeplitz(kappax)\kappayx)';

