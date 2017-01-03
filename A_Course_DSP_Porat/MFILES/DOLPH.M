function w = dolph(N,sll);
% Synopsis: w = dolph(N,sll).
% Computes a Dolph-Chebyshev window.
% Input parameters:
% N: the window length
% sll: the side-lobe level, in dB (negative number)
% Output:
% w: the window.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

b = 10^(-sll/20); b = cosh(log(b+sqrt(b^2-1))/(N-1));
c = b*cos((pi/N)*(0:N-1)); ind1 = find(abs(c) <= 1);
ind2 = find(abs(c) > 1); w = zeros(1,N);
w(ind1) = cos((N-1)*acos(c(ind1)));
w(ind2) = cosh((N-1)*log(c(ind2)+sqrt(c(ind2).^2-1))); w = real(w);
if (rem(N,2) == 0), w = w.*exp((j*pi/N)*(0:N-1)); end
w = fftshift(real(ifft(w))); w = (1/w(floor(N+1)/2))*w;

