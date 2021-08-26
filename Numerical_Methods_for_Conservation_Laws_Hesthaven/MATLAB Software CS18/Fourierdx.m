function [dudx] = Fourierdx(u,q);
% function [dudx] = Fourierdx(u,q);
% Purpose: Compute q'th derivative of u using a Fourier spectral approach
ii = sqrt(-1); N = length(u);
uhat = fft(u); nvec = (ii*[0:(N-1)/2 -(N-1)/2:-1]').^q;
dudx = real(ifft(nvec.*uhat));
return