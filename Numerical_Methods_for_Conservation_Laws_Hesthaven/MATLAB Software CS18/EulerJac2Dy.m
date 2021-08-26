function [Ay] = EulerJac2Dy(q,gamma);
% function [Ax] = EulerJac2Dy(q,gamma);
% Purpose: Evaluate y-flux Jacobian for 2D Euler equations
Ay = zeros(4);
r = q(1); ru = q(2); rv = q(3); E = q(4); u = ru/r; v = rv/r; 
p = (gamma-1)*(E - 0.5*(ru.^2 + rv.^2)./r); pE = 0.5*(gamma-1)*(u^2+v^2);

Ay(1,3)=1; Ay(2,1) = -u*v; Ay(2,2) = v; Ay(2,3)=u; Ay(3,1) = pE - v^2;
Ay(3,2) = -(1-gamma)*u; Ay(3,3) = (3-gamma)*v; Ay(3,4) = gamma-1;
Ay(4,1) = (2*pE - gamma*E/r)*v; Ay(4,2) = -(gamma-1)*u*v;
Ay(4,3) = gamma*E/r - pE - (gamma-1)*v^2; Ay(4,4)=gamma*v;
return