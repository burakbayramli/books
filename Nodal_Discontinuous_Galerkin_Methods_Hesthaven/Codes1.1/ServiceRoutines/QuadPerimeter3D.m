function P = QuadPerimeter3D(xq, yq, zq)

% function P = QuadPerimeter3D(xq, yq, zq)
% Purpose: find perimeter or quads given by coordinates
  
P1 = (xq(2,:)-xq(1,:)).^2 + (yq(2,:)-yq(1,:)).^2 + (zq(2,:)-zq(1,:)).^2;
P1 = sqrt(P1);

P2 = (xq(3,:)-xq(2,:)).^2 + (yq(3,:)-yq(2,:)).^2 + (zq(3,:)-zq(2,:)).^2;
P2 = sqrt(P2);

P3 = (xq(4,:)-xq(3,:)).^2 + (yq(4,:)-yq(3,:)).^2 + (zq(4,:)-zq(3,:)).^2;
P3 = sqrt(P3);

P4 = (xq(1,:)-xq(4,:)).^2 + (yq(1,:)-yq(4,:)).^2 + (zq(1,:)-zq(4,:)).^2;
P4 = sqrt(P4);

P = P1+P2+P3+P4;
return
