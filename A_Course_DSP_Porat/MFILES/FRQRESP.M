function H = frqresp(b,a,K,theta);
% Synopsis: H = frqresp(b,a,K,theta).
% Frequency response of b(z)/a(z) on a given frequency interval.
% Input parameters:
% b, a: numerator and denominator polynomials
% K: the number of frequency response points to compute
% theta: if absent, the K points are uniformly spaced on [0, pi];
%        if present and theta is a 1-by-2 vector, its entries are
%        taken as the end points of the interval on which K evenly
%        spaced points are placed; if the size of theta is different
%        from 2, it is assumed to be a vector of frequencies for which
%        the frequency response is to be computed, and K is ignored.
% Output:
% H: the frequency response vector.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

if (nargin == 3),
   H = fft(b,2*K-2)./fft(a,2*K-2); H = H(1:K);
elseif (length(theta) == 2),
   t0 = theta(1); dt = (theta(2)-theta(1))/(K-1);
   H = chirpf(b,t0,dt,K)./chirpf(a,t0,dt,K);
else
   H = zeros(1,length(theta));
   for i = 1:length(theta),
      H(i) = sum(b.*exp(-j*theta(i)*(0:length(b)-1)))/ ...
             sum(a.*exp(-j*theta(i)*(0:length(a)-1)));
   end
end

