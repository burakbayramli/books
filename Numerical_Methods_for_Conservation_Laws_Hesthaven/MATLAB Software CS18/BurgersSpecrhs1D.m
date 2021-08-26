function [du] = BurgersSpecrhs1D(x,u,L)
% function [du] = BurgersSpecrhs1D(x,u,L)
% Purpose  : Evaluate the RHS of Burgers equations using spectral Fourier
% collocation method

% Compute redidual - direct form
du = - 2*pi/L*Fourierdx(u.^2,1);

% Compute residual - skew symmetric form
% du = - 2*pi/L*2/3*(u.*Fourierdx(u,1) + Fourierdx(u.^2,1));
return