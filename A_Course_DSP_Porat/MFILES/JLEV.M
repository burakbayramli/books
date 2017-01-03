function b = jlev(kappax,kappayx);
% Synopsis: b = jlev(kappax,kappayx).
% The joint Levinson algorithm.
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
a = 1; s = kappax(1); b = kappayx(1)/kappax(1);
for i = 1:q,
   rho = (a*kappax(i+1:-1:2))/s;
   s = s*(1-rho^2);
   a = [a,0]; a = a - rho*fliplr(a);
   bii = (kappayx(i+1) - b*kappax(i+1:-1:2))/s;
   b = [b+bii*fliplr(a(2:i+1)), bii];
end

