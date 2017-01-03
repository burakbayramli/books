function h = diffhilb(typ,N,win);
% Synopsis: h = diffhilb(typ,N,win).
% Design of a FIR differentiator or a FIR Hilbert transformer
% by truncated impulse response, with optional windowing.
% Input parameters:
% typ: 'd' for differentiator, 'b' for Hilbert
% N: the filter order (the number of coefficients is N+1)
% win: an optional window.
% Output:
% h: the filter coefficients.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

flag = rem(N,2); n = (0:N)-(N/2); if (~flag), n(N/2+1) = 1; end
if (typ == 'd'),
   if (~flag), h = ((-1).^n)./n; h(N/2+1) = 0;
   else, h = ((-1).^(round(n+0.5)))./(pi*n.^2); end
elseif (typ == 'b'),
   h = (1-cos(pi*n))./(pi*n); if (~flag), h(N/2+1) = 0; end, end
if  nargin == 3, h = h.*win; end

