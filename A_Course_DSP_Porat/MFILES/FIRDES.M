function h = firdes(N,spec,win);
% Synopsis: h = firdes(N,spec,win).
% Design of a general multiband FIR filter by truncated
% impulse response, with optional windowing.
% Input parameters:
% N: the filter order (the number of coefficients is N+1)
% spec: a table of K rows and 3 columns, a row to a band:
%       spec(k,1) is the low cutoff frequency,
%       spec(k,2) is the high cutoff frequency,
%       spec(k,3) is the gain.
% win: an optional window of length N+1.
% Output:
% h: the impulse response coefficients.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

flag = rem(N,2); [K,m] = size(spec);
n = (0:N) - N/2; if (~flag), n(N/2+1) = 1; end, h = zeros(1,N+1);
for k = 1:K,
   temp = (spec(k,3)/pi)*(sin(spec(k,2)*n) - sin(spec(k,1)*n))./n;
   if (~flag), temp(N/2+1) = spec(k,3)*(spec(k,2) - spec(k,1))/pi; end
   h = h + temp; end
if  nargin == 3, h = h.*reshape(win,1,N+1); end

