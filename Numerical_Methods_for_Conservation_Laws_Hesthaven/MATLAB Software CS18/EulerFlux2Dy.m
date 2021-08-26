function [qy] = EulerFlux2Dy(q,gamma);
% function [qy] = EulerFlux2Dy(q,gamma);
% Purpose: Evaluate y-flux for 2D Euler equations
r = q(:,1); ru = q(:,2); rv = q(:,3); E = q(:,4);
p = (gamma-1)*(E - 0.5*(ru.^2 + rv.^2)./r);

qy(:,1)= rv; qy(:,2) = ru.*rv./r;
qy(:,3) = rv.^2./r+p; qy(:,4) = (E+p).*rv./r;
return

