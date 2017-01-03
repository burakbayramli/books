function D = grpdly(b,a,K,theta);
% Synopsis: D = grpdly(b,a,K,theta).
% Group delay of b(z)/a(z) on a given frequency interval.
% Input parameters:
% b, a: numerator and denominator polynomials
% K: the number of frequency response points to compute
% theta: if absent, the K points are uniformly spaced on
%        [0, pi]; if present and theta is a 1-by-2 vector,
%        its entries are taken as the end points of the
%        interval on which K evenly spaced points are
%        placed; if the size of theta is different from 2,
%        it is assumed to be a vector of frequencies for
%        which the group delay is to be computed, and K is
%        ignored.
% Output:
% D: the group delay vector.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

a = reshape(a,1,length(a)); b = reshape(b,1,length(b));
if (length(a) == 1), % case of FIR
   bd = -j*(0:length(b)-1).*b;
   if (nargin == 3),
      B = frqresp(b,1,K); Bd = frqresp(bd,1,K);
   else,
      B = frqresp(b,1,K,theta); Bd = frqresp(bd,1,K,theta);
   end
   D = (real(Bd).*imag(B)-real(B).*imag(Bd))./abs(B).^2;
else % case of IIR
   if (nargin == 3), D = grpdly(b,1,K)-grpdly(a,1,K);
   else, D = grpdly(b,1,K,theta)-grpdly(a,1,K,theta); end
end

