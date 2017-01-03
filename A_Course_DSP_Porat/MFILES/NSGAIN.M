function ng = nsgain(b,a);
% Synopsis: ng = nsgain(b,a).
% Computes the noise gain of a rational system b(z)/a(z).
% Input parameters:
% b, a: the numerator and denominator coefficients.
% Output:
% ng: the noise gain

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

p = length(a)-1; q = length(b)-1; n = max(p,q);
if (p == 0), ng = sum(b.^2); return, end
a = [reshape(a,1,p+1),zeros(1,n-p)];
b = [reshape(b,1,q+1),zeros(1,n-q)];
mat = toeplitz([1; zeros(n,1)],a) + ...
      hankel(a',[a(n+1),zeros(1,n)]);
vec = toeplitz([b(1); zeros(n,1)],b)*b';
vec = mat\vec; ng = 2*vec(1);

