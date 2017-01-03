function [v,u,C] = elliplp(N,w0,epsilon,m);
% Synopsis: [v,u,C] = elliplp(N,w0,epsilon,m).
% Designs a low-pass elliptic filter.
% Input parameters:
% N: the order
% w0: the pass-band edge
% epsilon, m: filter parameters.
% Output parameters:
% v, u, C: poles, zeros, and constant gain of the filter.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

flag = rem(N,2); K = ellipke(m);
if (~flag), lmax = N/2; l = (1:lmax)-0.5;
else, lmax = (N-1)/2; l = 1:lmax; end
zl = ellipj((2*K/N)*l,m); pl = 1 ./(sqrt(m)*zl);
f = prod((1-pl.^2)./(1-zl.^2));
u = w0*reshape([j*pl; -j*pl],1,2*lmax);
a = 1;
for l = 1:lmax,
   for i = 1:2, a = conv(a,[1,0,pl(l)^2]); end
end
b = 1;
for l = 1:lmax,
   for i = 1:2, b = conv(b,[1,0,zl(l)^2]); end
end
b = (f*epsilon)^2*b;
if (flag), b = -[b,0,0]; a = [0,0,a]; end
v = roots(a+b).'; v = w0*v(find(real(v) < 0));
C = prod(-v)./prod(-u);
if (~flag), C = C/sqrt(1+epsilon^2); end, C = real(C);

