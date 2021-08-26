function [S, iS, Lam] = EulerChar(q0,gamma);
% Purpose: Compute characteristic decomposition for Euler equations at q0
r0 = q0(1); ru0 = q0(2); E0 = q0(3); u0 = ru0/r0;
p0 = (gamma-1)*(E0 - 0.5*ru0.^2/r0); c0 = sqrt(gamma*p0./r0); 
H0 = (E0+p0)/r0;

S = zeros(3,3); iS = zeros(3,3); Lam = zeros(3,3);

S(1,1)=1/(2*c0); S(1,2)=1.0; S(1,3)=1/(2*c0);
S(2,1)=(u0+c0)/(2*c0); S(2,2)=u0; S(2,3)=(u0-c0)/(2*c0);
S(3,1)=(H0+c0*u0)/(2*c0); S(3,2)=u0^2/2; S(3,3)=(H0-c0*u0)/(2*c0);

iS(1,1)=(0.5*(gamma-1)*u0^2-c0*u0)/c0; 
iS(1,2)=-((gamma-1)*u0-c0)/c0; 
iS(1,3)=(gamma-1)/c0;
iS(2,1)=1-0.5*(gamma-1)*u0^2/c0^2; 
iS(2,2)=(gamma-1)/c0^2*u0; 
iS(2,3) =-(gamma-1)/c0^2;
iS(3,1)=(0.5*(gamma-1)*u0^2+c0*u0)/c0; 
iS(3,2)=-((gamma-1)*u0+c0)/c0; 
iS(3,3)=(gamma-1)/c0;

Lam(1,1) = u0+c0; Lam(2,2) = u0; Lam(3,3) = u0-c0;
return