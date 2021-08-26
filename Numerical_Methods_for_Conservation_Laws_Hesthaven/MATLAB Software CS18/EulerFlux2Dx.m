function [qx] = EulerFlux2Dx(q,gamma);
% function [qx] = EulerFlux2Dx(q,gamma);
% Purpose: Evaluate x-flux for 2D Euler equations
r = q(:,1); ru = q(:,2); rv = q(:,3); E = q(:,4);
p = (gamma-1)*(E - 0.5*(ru.^2 + rv.^2)./r);

qx(:,1)= ru; qx(:,2) = ru.^2./r + p;
qx(:,3) = ru.*rv./r; qx(:,4) = (E+p).*ru./r;
return

