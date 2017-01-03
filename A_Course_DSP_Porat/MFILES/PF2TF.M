function [b,a] = pf2tf(c,A,alpha);
% Synopsis: [b,a] = pf2tf(c,A,alpha).
% Conversion of partial fraction decomposition to the form b(z)/a(z).
% The polynomials are in negative powers of z.
% Input parameters:
% c: the free polynomial; empty if deg(b) < deg(a)
% A: the vector of gains of the partial fractions
% alpha: the vector of poles.
% Output parameters:
% a, b: the output polynomials

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

p = length(alpha); d = A(1); a = [1,-alpha(1)];
for k = 2:p,
   d = conv(d,[1,-alpha(k)]) + A(k)*a;
   a = conv(a,[1,-alpha(k)]);
end
if (length(c) > 0),
   b = conv(c,a) + [d,zeros(1,length(c))];
else, b = d; end
a = real(a); b = real(b);

