function [b,a,v,u,C] = analoglp(typ,N,w0,epsilon,m);
% Synopsis: [b,a,v,u,C] = analoglp(typ,N,w0,epsilon,m).
% Butterworth, Chebyshev-I or Chebyshev-II low-pass filter.
% Input parameters:
% typ: filter type: 'but', 'ch1', 'ch2', or 'ell'
% N: the filter order
% w0: the frequency parameter
% epsilon: the tolerance parameter; not needed for Butterworth
% m: parameter needed for elliptic filters.
% Output parameters:
% b, a: numerator and denominator polynomials
% v, u, C: poles, zeros, and constant gain.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

if (typ == 'ell'),
   [v,u,C] = elliplp(N,w0,epsilon,m);
   a = 1; for i = 1:N, a = conv(a,[1, -v(i)]); end
   b = C; for i = 1:length(u), b = conv(b,[1, -u(i)]); end
   a = real(a); b = real(b); C = real(C); return
end
k = (0.5*pi/N)*(1:2:2*N-1); s = -sin(k); c = cos(k);
if (typ == 'but'), v  = w0*(s+j*c);
elseif (typ(1:2) == 'ch'),
   f = 1/epsilon; f = log(f+sqrt(1+f^2))/N;
   v = w0*(sinh(f)*s+j*cosh(f)*c);
end
if (typ == 'ch2'), 
   v = (w0^2)./v;
   if (rem(N,2) == 0), u = j*w0./c;
   else, u = j*w0./[c(1:(N-1)/2),c((N+3)/2:N)]; end
end
a = 1; for k = 1:N, a = conv(a,[1, -v(k)]); end
if (typ == 'but' | typ == 'ch1'),
   C = prod(-v); b = C; u = [];
elseif (typ == 'ch2'), 
   C = prod(-v)/prod(-u); b = C;
   for k = 1:length(u), b = conv(b,[1, -u(k)]); end;
end
if (typ == 'ch1' & rem(N,2) == 0),
   f = (1/sqrt(1+epsilon^2)); b = f*b; C = f*C; end
a = real(a); b = real(b); C = real(C);

