function x = invz(b,a,N);
% Synopsis: x = invz(b,a,N).
% Computes first N terms of the inverse z-transform
% of the rational transfer function b(z)/a(z).
% The poles are assumed distinct.
% Input parameters:
% b, a: numerator and denominator input polynomials
% N: number of points to be computed
% Output:
% x: the inverse sequence.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

[c,A,alpha] = tf2pf(b,a);
x = zeros(1,N);
x(1:length(c)) = c;
for k = 1:length(A),
   x = x+A(k)*(alpha(k)).^(0:N-1);
end
x = real(x);

