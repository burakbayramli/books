function [dUdx, dUdy, dUdz] = GradH3D(U)

% function [dUdx, dUdy, dUdz] = GradH3D(U)
% purpose: compute local elemental physical spatial derivatives of U
    
Globals3D;

% compute local derivatives on reference tetrahedron  
dUdr = Dr*U;  dUds = Ds*U;  dUdt = Dt*U;

% compute physical spatial derivatives using the chain rule
dUdx = rx.*dUdr + sx.*dUds + tx.*dUdt;
dUdy = ry.*dUdr + sy.*dUds + ty.*dUdt;
dUdz = rz.*dUdr + sz.*dUds + tz.*dUdt;
return;
