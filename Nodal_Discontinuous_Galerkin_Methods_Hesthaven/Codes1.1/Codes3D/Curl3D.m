function [curlx, curly, curlz] = CurlH3D(Ux, Uy, Uz)

% function [curlx, curly, curlz] = CurlH3D(Ux, Uy, Uz)
% purpose: compute local elemental physical spatial curl of (Ux,Uy,Uz)

Globals3D;

% compute local derivatives of Ux on reference tetrahedron  
ddr = Dr*Ux;  dds = Ds*Ux;  ddt = Dt*Ux;

% increment curl components
curly =  (rz.*ddr + sz.*dds + tz.*ddt);
curlz = -(ry.*ddr + sy.*dds + ty.*ddt);

% compute local derivatives of Uy on reference tetrahedron  
ddr = Dr*Uy;  dds = Ds*Uy;  ddt = Dt*Uy;

% increment curl components
curlx =         -(rz.*ddr + sz.*dds + tz.*ddt);
curlz =  curlz + (rx.*ddr + sx.*dds + tx.*ddt);

% compute local derivatives of Uz on reference tetrahedron  
ddr = Dr*Uz;  dds = Ds*Uz;  ddt = Dt*Uz;

% increment curl components
curlx =  curlx + (ry.*ddr + sy.*dds + ty.*ddt);
curly =  curly - (rx.*ddr + sx.*dds + tx.*ddt);
return;
