function dtscale = dtscale3D; 

% function dtscale = dtscale3D;
% Purpose : Compute ratio between volume and face Jacobian as characteristic
%           for grid to choose timestep

Globals3D;

dtscale = 1./(max(max(Fscale))*(N)*(N));
return;
