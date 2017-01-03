function [dHmag,S] = sensiir(typ,b,a,K,theta);
% Synopsis: [dHmag,S] = sensiir(typ,b,a,K,theta).
% Computes the sensitivity bound for the magnitude response of
% an IIR filter to coefficient quantization.
% Input parameters:
% typ: 'd' for direct realization
%      'p' for parallel realization
%      'c' for cascade realization
% b, a: numerator and denominator polynomials
% K: number of frequency points
% theta: frequency interval (2-element vector).
% Output parameters:
% dHmag: the partial derivative matrix, M by K, where M is the
%        number of coefficients in the realization
% S: the sensitivity bound, 1 by K.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

Hangle = exp(-j*angle(frqresp(b,a,K,theta)));
if (typ == 'd'),
   [dH,sc] = dhdirect(b,a,K,theta);
elseif (typ == 'p'),
   [c,nsec,dsec] = tf2rpf(b,a);
   [dH,sc] = dhparal(nsec,dsec,c,K,theta);
elseif (typ == 'c'),
   c = b(1); v = roots(a); u = roots(b);
   [nsec,dsec] = pairpz(v,u);
   [dH,sc] = dhcascad(nsec,dsec,c,K,theta);
end
[M,junk] = size(dH);
dHmag = real(dH.*(ones(M,1)*Hangle));
S = sum(abs((sc*ones(1,K)).*dHmag));

