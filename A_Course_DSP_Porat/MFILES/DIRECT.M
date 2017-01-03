function y = direct(typ,b,a,x);
% Synopsis: direct(typ,b,a,x).
% Direct realizations of rational transfer functions.
% Input parameters:
% typ: 1 for direct realization, 2 for transposed
% b, a: numerator and denominator polynomials
% x: input sequence.
% Output:
% y: output sequence.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

p = length(a)-1; q = length(b)-1; pq = max(p,q);
a = a(2:p+1); u = zeros(1,pq); % u: the internal state
if (typ == 1),
   for i = 1:length(x),
      unew = x(i)-sum(u(1:p).*a);
      u = [unew,u];
      y(i) = sum(u(1:q+1).*b);
      u = u(1:pq);
   end
elseif (typ == 2),
   for i = 1:length(x),
      y(i) = b(1)*x(i)+u(1);
      u = [u(2:pq),0];
      u(1:q) = u(1:q) + b(2:q+1)*x(i);
      u(1:p) = u(1:p) - a*y(i);
   end
end

