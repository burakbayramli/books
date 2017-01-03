function [N,m] = ellord(k,d);
% Synopsis: [N,m] = ellord(k,d).
% Finds the order and the parameter m of an elliptic filter.
% Input parameters:
% k, d: the selectivity and discrimination factors.
% Output parameters:
% N: the order
% m: the parameter for the Jacobi elliptic function.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

m0 = k^2; C = ellipke(1-d^2)/ellipke(d^2);
N0  = C*ellipke(m0)/ellipke(1-m0);
if (abs(N0-round(N0))) <= 1.0e-6,
   N = round(N0); m = m0; return; end
N = ceil(N0); m = 1.1*m0;
while (C*ellipke(m)/ellipke(1-m) < N), m = 1.1*m; end
N1 = C*ellipke(m)/ellipke(1-m);
while (abs(N1-N) >= 1.0e-6),
   a = (N1-N0)/(m-m0);
   mnew = m0+(N-N0)/a;
   Nnew = C*ellipke(mnew)/ellipke(1-mnew);
   if (Nnew < N), m0 = mnew; N0 = Nnew;
   else, m = mnew; N1 = Nnew; end
end

