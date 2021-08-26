function [dq] = EulerSpecrhs1D(x,q,L,gamma)
% function [dq] = EulerSpecrhs1D(x,q,L,gamma)
% Purpose  : Evaluate the RHS of the Euler equations using spectral Fourier
% collocation method
r = q(:,1); ru = q(:,2); E = q(:,3); p = (gamma-1)*(E-0.5*ru.^2./r);

% Compute right hand side
dq(:,1) = -2*pi/L*Fourierdx(ru,1);
dq(:,2) = -2*pi/L*Fourierdx(ru.^2./r+p,1);
dq(:,3) = -2*pi/L*Fourierdx((E+p).*ru./r,1);
return