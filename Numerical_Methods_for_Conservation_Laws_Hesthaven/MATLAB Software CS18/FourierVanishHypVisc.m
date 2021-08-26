function [Fu] = FourierVanishHypVisc(u,s,k,N,L);
% function [Fu] = FourierVanishHypVisc(u,s,k,N,L);
% Purpose: Apply vanishing viscosity as a filter

% Set parameters in vanishing viscosity model
theta = 0.9*(2*s-1)/(2*s); mN = floor((L/(2*pi)*N)^theta); 
C=0.5*(2*pi/L)^(2*s);

% Define vanishing viscosity by a filter function
qhat = zeros(N+1,1); qhat((mN+1):(N+1)) = 1 - (mN./[mN:N]').^((2*s-1)/theta);
sigma = exp(-C*N*k*(([0:N]')/N).^(2*s).*qhat);
nvec = [sigma; flipud(sigma(2:end))];
Fu = real(ifft(nvec.*fft(u)));
return