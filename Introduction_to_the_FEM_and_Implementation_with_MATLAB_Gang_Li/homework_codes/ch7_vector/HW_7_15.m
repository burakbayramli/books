% next block: nodal displacements
uvw=[0.1  -0.2  -0.2  -0.3  0.05  0 ...
 -0.1  -0.1  -0.1  0.04  0  0.3 ]'*0.1;

% next block: points of interest
pts=[0 1 0
     0 0 1];
[N,Nx,Ny,Nz]=CompNDNatPointsTetra4(pts(:,1), pts(:,2), pts(:,3));

% next block: element nodal coordinates
element_nodes=[10 8 10
                20 8 10
                18 8 0
                16 20 4];
% for-loop: compute strains
for j=1:2
  J=CompJacobian3DatPoint(element_nodes, Nx(:,j), Ny(:,j), Nz(:,j));
  detJ=det(J);
  Jinv=inv(J);
  B=CompB6x12Tetra4atPoint(Nx(:,j), Ny(:,j), Nz(:,j), Jinv);
  ep=B*uvw
end
