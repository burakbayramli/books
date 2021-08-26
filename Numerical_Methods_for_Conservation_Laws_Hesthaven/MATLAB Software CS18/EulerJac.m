function [A] = EulerJac(q,gamma);
% function [A] = EulerJac(q,gamma);
% Purpose: Compute flux Jacobian for 1D Euler equations
A = zeros(3,3); r = q(1); ru = q(2); E = q(3); u = ru/r;
A(2,1) = -(3-gamma)/2*u^2; A(2,2) = (3-gamma)*u; A(2,3) = gamma-1;
A(1,2)=1; A(3,1) = -gamma*E*u/r + (gamma-1)*u^3; 
A(3,2) = gamma*E/r - 3*(gamma-1)/2*u^2; A(3,3) = gamma*u;
return