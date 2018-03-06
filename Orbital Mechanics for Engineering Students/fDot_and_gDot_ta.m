% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  function [fdot, gdot] = fDot_and_gDot_ta(r0, v0, dt, mu)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%{
  This function calculates the time derivatives of the Lagrange
  f and g coefficients from the change in true anomaly since time t0.

  mu   - gravitational parameter (km^3/s^2)
  dt   - change in true anomaly (degrees)
  r0   - position vector at time t0 (km)
  v0   - velocity vector at time t0 (km/s)
  h    - angular momentum (km^2/s)
  vr0  - radial component of v0 (km/s)
  fdot - time derivative of the Lagrange f coefficient (1/s)
  gdot - time derivative of the Lagrange g coefficient (dimensionless)

  User M-functions required:  None
%}
% --------------------------------------------------------

h   = norm(cross(r0,v0));
vr0 = dot(v0,r0)/norm(r0);
r0  = norm(r0);
c   = cosd(dt);
s   = sind(dt);

%...Equations 2.158c & d:
fdot = mu/h*(vr0/h*(1 - c) - s/r0);
gdot = 1 - mu*r0/h^2*(1 - c);

end
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~