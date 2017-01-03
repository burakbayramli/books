function X = chirpf(x,theta0,dtheta,K);
% Synopsis: X = chirpf(x,theta0,dtheta,K).
% Computes the chirp Fourier transform on a frequency
% interval.
% Input parameters:
% x  : the input vector
% theta0 : initial frequency (in radians)
% dtheta : frequency increment (in radians)
% K  : number of points on the frequency axis.
% Output:
% X: the chirp Fourier transform of x.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

N = length(x); x = reshape(x,1,N); n = 0:N-1;
g = x.*exp(-j*(0.5*dtheta*n+theta0).*n);
L = 1; while (L < N+K-1), L = 2*L; end
g = [g, zeros(1,L-N)];
h = [exp(j*0.5*dtheta*(0:K-1).^2), ...
     exp(j*0.5*dtheta*(-L+K:-1).^2)];
X = ifft(fft(g).*fft(h));
X = X(1:K).*exp(-j*0.5*dtheta*(0:K-1).^2);

