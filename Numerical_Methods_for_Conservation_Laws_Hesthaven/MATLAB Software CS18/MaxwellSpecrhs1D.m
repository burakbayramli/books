function [dq] = MaxwellSpecrhs1D(x,q,ep,mu,L);
% function [dq] = MaxwellSpecrhs1D(x,q,ep,mu,L)
% Purpose: Evaluate right hand side for Maxwells equation using spectral
% Fourier collocation method

N = length(x); dq = zeros(N,2);
dq(:,1) = 2*pi/L*Fourierdx(q(:,2),1)./ep;
dq(:,2) = 2*pi/L*Fourierdx(q(:,1),1)./mu;
return