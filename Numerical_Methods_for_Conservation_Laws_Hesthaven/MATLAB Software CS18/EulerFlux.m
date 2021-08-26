function [fu] = EulerFlux(q,gamma);
% function [fu] = EulerFlux(q,gamma);
% Purpose: Compute flux for 1D Euler equations
N = length(q(:,1)); fu = zeros(N,3);

r = q(:,1); ru = q(:,2); E = q(:,3); 
fu(:,1) = ru; p = (gamma-1)*(E - 0.5*ru.^2./r);
fu(:,2) = ru.^2./r + p; fu(:,3) = (E+p).*ru./r;
return