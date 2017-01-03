%5.4  CCintegrationcode.m

x = cos(pi*(0:n)'/n); fx = feval(f,x)/(2*n); % f(x) at Chebyshev points
g = real(fft(fx([1:n+1  n:-1:2])));         % the FFT gives garbled a_k's
A = [g(1); g(2:n) + g(2*n:-1:n+2); g(n+1)]; % cosine coefficients for Sigma''
w = 0*x'; w(1:2:end) = 2./(1 - (0:2:n).^2); % integrals = weights for (17)
I = w * A;  % Clenshaw-Curtis quadrature is exact when f has degree n + 1
