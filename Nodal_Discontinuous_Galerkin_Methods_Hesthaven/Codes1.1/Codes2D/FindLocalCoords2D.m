function [rOUT,sOUT] = FindLocalCoords2D(k, xout, yout)

% function [rOUT,sOUT] = FindLocalCoords2D(k, xout, yout)
% Purpose: find local (r,s) coordinates in the k'th element of given coordinates
%          [only works for straight sided triangles]

Globals2D;
  
v1  = EToV(k,1); v2  = EToV(k,2); v3  = EToV(k,3);
xy1 = [VX(v1);VY(v1)];  xy2 = [VX(v2);VY(v2)];  xy3 = [VX(v3);VY(v3)];

A(:,1) = xy2-xy1;  A(:,2) = xy3-xy1;

rOUT = zeros(size(xout));  sOUT = zeros(size(xout));

for i=1:length(xout(:))
  rhs = 2.0*[xout(i);yout(i)] -xy2 -xy3;
  
  tmp = A\rhs;
  rOUT(i) = tmp(1); sOUT(i) = tmp(2);
end
return
