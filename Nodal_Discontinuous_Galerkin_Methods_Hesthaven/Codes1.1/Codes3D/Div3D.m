function [divU] = DivH3D(Ux, Uy, Uz)

% function [divU] = DivH3D(Ux, Uy, Uz)
% Purpose: compute local elemental physical spatial divergence of (Ux,Uy,Uz)

Globals3D;

% compute local derivatives of Ux on reference tetrahedron  
ddr = Dr*Ux;  dds = Ds*Ux;  ddt = Dt*Ux;

% dUx/dx
divU =  (rx.*ddr + sx.*dds + tx.*ddt);

% compute local derivatives of Uy on reference tetrahedron  
ddr = Dr*Uy;  dds = Ds*Uy;  ddt = Dt*Uy;

% add dUy/dy to divergence
divU =  divU + (ry.*ddr + sy.*dds + ty.*ddt);

% compute local derivatives of Uz on reference tetrahedron  
ddr = Dr*Uz;  dds = Ds*Uz;  ddt = Dt*Uz;

% add dUz/dz to divergence
divU =  divU + (rz.*ddr + sz.*dds + tz.*ddt);
return;
