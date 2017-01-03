function [g0,g1,h0,h1] = cqfw(w);
% Synopsis: [g0,g1,h0,h1] = cqfw(w).
% Designs a Smith-Barnwell CQF filter bank by windowing.
% Input parameters:
% w: the window; must have even length, which will also be
%    the length of the filters.
% Output parameters:
% g0, g1: the analysis filters
% h0, h1: the synthesis filters

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

N = length(w)-1; % order of the output filters
w = conv(w,w); w = (1/w(N+1))*w;
r = firdes(length(w)-1,[0,0.5*pi,1],w);
rr = roots(r);
g0 = real(poly(rr(find(abs(rr) < 1))));
g0 = reshape(g0/sqrt(2*sum(g0.^2)),1,N+1);
h1 = (-1).^(0:N).*g0; g1 = fliplr(h1);
h0 = 2*fliplr(g0); h1 = 2*h1;

