function [Ax] = EulerJac2Dx(q,gamma);
% function [Ax] = EulerJac2Dx(q,gamma);
% Purpose: Evaluate x-flux Jacobian for 2D Euler equations
Ax = zeros(4);
r = q(1); ru = q(2); rv = q(3); E = q(4); u = ru/r; v = rv/r; 
p = (gamma-1)*(E - 0.5*(ru.^2 + rv.^2)./r); pE = 0.5*(gamma-1)*(u^2+v^2);

Ax(1,2)=1; Ax(2,1) = pE - u^2; Ax(2,2) = (3-gamma)*u; Ax(3,1) = -u*v; 
Ax(2,3) = -(gamma-1)*v; Ax(2,4) = gamma-1; Ax(3,2) = v; Ax(3,3)=u;
Ax(4,1) = (2*pE - gamma*E/r)*u; Ax(4,2) = gamma*E/r - pE - (gamma-1)*u^2; 
Ax(4,3) = -(gamma-1)*u*v; Ax(4,4)=gamma*u;
return