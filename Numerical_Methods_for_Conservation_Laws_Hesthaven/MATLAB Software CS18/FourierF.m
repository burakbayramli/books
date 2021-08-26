function [Fu] = FourierF(u,p,alpha,N0);
% function [Fu] = FourierF(u,alpha,N0);
% Purpose: Apply modal exponential filter in a Fourier spectral approach;
N = length(u); N2 = (N-1)/2;

% Define filter function
sigma = ones(N2+1,1); 
sigma((N0+1):(N2+1)) = exp(-alpha*(([N0:N2]'-N0)/(N2-N0)).^(2*p));
nvec = [sigma; flipud(sigma(2:end))];
Fu = real(ifft(nvec.*fft(u)));
return