function [theta,val] = maxdft(x,M,name,alpha);
% Synopsis: [theta,val] = maxdft(x,M,name,alpha).
% Finds the M largest maxima of the DFT of the real vector x.
% Input parameters:
% x: the input vector
% M: number of local maxima to be found
% name: an optional window for x; one of the names in window.m
% alpha: needed if name = 'kaiser'.
% Output parameters:
% theta: vector of thetas at the local maxima
% val: vector of corresponding values of DFT(x).

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

N = length(x); x = reshape(x,1,N);
if (nargin == 3), x = x.*window(N,name);
elseif (nargin == 4), x = x.*window(N,name,alpha); end
X = abs(fft(x)); X = X(1:floor((N+1)/2));
[y,ind] = locmax(X); ind = ind(1:M);
theta = (2*pi/N)*(ind-1); val = zeros(1,M);
for m = 1:M,
   X = chirpf(x,theta(m)-(2*pi/N),(2*pi/N^2),2*N+1); 
   [y,ind] = max(abs(X));
   theta(m) = theta(m)-(2*pi/N) + (ind-1)*2*pi/N^2;
   val(m) = y;
end

