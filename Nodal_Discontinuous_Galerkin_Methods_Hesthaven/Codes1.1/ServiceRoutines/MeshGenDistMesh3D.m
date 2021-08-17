function [Nv, VX, VY, VZ, K, EToV] = MeshGenDistMesh3D(h0)

% function [VX, VY, VZ, K, EToV] = MeshGenDistMesh3D(h0)
% Purpose  : Generate 3D square mesh using DistMesh;
% By Allan P. Engsig-Karup

% Parameters to set/define
%    fd     Distance function for mesh boundary
%    fh     Weighting function for distributing elements
%    h0     Characteristic length of elements
%    Bbox   Bounding box for mesh
%    param  Parameters to be used in function call with DistMesh

%fd = inline('dblock(p,-1,1,-1,1,-1,1)','p');
fh = @huniform;

fd=inline('sqrt(sum(p.^2,2))-1','p');

Bbox = [-1 -1 -1; 1 1 1];
Fix  = [-1 -1 -1; 1 -1 -1; 1 1 -1; -1 1 -1;...
        -1 -1  1; 1 -1  1; 1 1  1; -1 1  1];
param = [];

% Call distmesh
% [Vert,EToV]=distmeshnd(fd,fh,h0,Bbox, Fix);
[Vert,EToV]=distmeshnd(fd,fh,h0,[-1,-1,-1;1,1,1],[]);

VX = Vert(:,1)'; VY = Vert(:,2)'; VZ = Vert(:,3)';
Nv = length(VX); K  = size(EToV,1);

% Reorder elements to ensure counter clockwise orientation
ax = VX(EToV(:,1)); ay = VY(EToV(:,1));  az = VZ(EToV(:,1));
bx = VX(EToV(:,2)); by = VY(EToV(:,2));	 bz = VZ(EToV(:,2));
cx = VX(EToV(:,3)); cy = VY(EToV(:,3));	 cz = VZ(EToV(:,3));
dx = VX(EToV(:,4)); dy = VY(EToV(:,4));	 dz = VZ(EToV(:,4));

Dx = (cy-ay).*(dz-az) - (cz-az).*(dy-ay);
Dy = (cz-az).*(dx-ax) - (cx-ax).*(dz-az);
Dz = (cx-ax).*(dy-ay) - (cy-ay).*(dx-ax);

D = (bx-ax).*Dx + (by-ay).*Dy + (bz-az).*Dz;

i = find(D<0);
EToV(i,:) = EToV(i,[1 2 4 3]);
return
