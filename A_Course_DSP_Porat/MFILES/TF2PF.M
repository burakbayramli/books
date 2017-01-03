function [c,A,alpha] = tf2pf(b,a);
% Synopsis: [c,A,alpha] = tf2pf(b,a).
% Partial fraction decomposition of b(z)/a(z). The polynomials are in
% negative powers of z. The poles are assumed distinct.
% Input parameters:
% a, b: the input polynomials
% Output parameters:
% c: the free polynomial; empty if deg(b) < deg(a)
% A: the vector of gains of the partial fractions
% alpha: the vector of poles.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

% Compute c(z) and d(z)
p = length(a)-1; q = length(b)-1;
a = (1/a(1))*reshape(a,1,p+1);
b = (1/a(1))*reshape(b,1,q+1);
if (q >= p), % case of nonempty c(z)
   temp = toeplitz([a,zeros(1,q-p)]',[a(1),zeros(1,q-p)]);
   temp = [temp,[eye(p); zeros(q-p+1,p)]];
   temp = temp\b';
   c = temp(1:q-p+1)'; d = temp(q-p+2:q+1)';
else
   c = []; d = [b,zeros(1,p-q-1)];
end

% Compute A and alpha
alpha = cplxpair(roots(a)).'; A = zeros(1,p);
for k = 1:p,
   temp = prod(alpha(k)-alpha(find(1:p ~= k)));
   if (temp == 0), error('Repeated roots in TF2PF');
   else, A(k) = polyval(d,alpha(k))/temp; end
end

