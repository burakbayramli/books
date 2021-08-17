function [vx,vy,vz] = Curl2D(ux,uy,uz);

% function [vx,vy,vz] = Curl2D(ux,uy,uz);
% Purpose: Compute 2D curl-operator in (x,y) plane

Globals2D;

uxr = Dr*ux; uxs = Ds*ux; uyr = Dr*uy; uys = Ds*uy; 
vz =  rx.*uyr + sx.*uys - ry.*uxr - sy.*uxs;
vx=[]; vy=[];

if (~isempty(uz));
  uzr = Dr*uz; uzs = Ds*uz;
  vx =  ry.*uzr + sy.*uzs; vy = -rx.*uzr - sx.*uzs;
end;
return
