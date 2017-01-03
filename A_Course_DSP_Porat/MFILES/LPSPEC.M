function [N,w0,epsilon,m] = lpspec(typ,wp,ws,deltap,deltas);
% Synopsis: [N,w0,epsilon,k,q] = lpspec(typ,wp,ws,deltap,deltas).
% Butterworth, Chebyshev-I or Chebyshev-II low-pass filter
% parameter computation from given specifications.
% Input parameters:
% typ: the filter type:
%      'but' for Butterworth
%      'ch1' for Chebyshev-I
%      'ch2' for Chebyshev-II
%      'ell' for elliptic
% wp, ws: band-edge frequencies
% deltap, deltas: pass-band and stop-band tolerances.
% Output parameters:
% N: the filter order
% w0: the frequency parameter
% epsilon: the tolerance parameter; not supplied for Butterworth
% m: parameter supplied in case of elliptic filter.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

d = sqrt(((1-deltap)^(-2)-1)/(deltas^(-2)-1)); di = 1/d;
k = wp/ws; ki =1/k;
if (typ == 'but'),
   N = ceil(log(di)/log(ki));
   w0 = wp*((1-deltap)^(-2)-1)^(-0.5/N);
   nargout = 2;
elseif (typ(1:2) == 'ch'),
   N = ceil(log(di+sqrt(di^2-1))/log(ki+sqrt(ki^2-1)));
   nargout = 3;
   if (typ(3) == '1'),
      w0 = wp; epsilon = sqrt((1-deltap)^(-2)-1);
   elseif (typ(3) == '2'),
      w0 = ws; epsilon = 1/sqrt(deltas^(-2)-1);
   end
elseif (typ == 'ell'),
   w0 = wp; epsilon = sqrt((1-deltap)^(-2)-1);
   [N,m]  = ellord(k,d);
   nargout = 4;
end

