% next block: displacements
uvw=[-0.01  0.02  0.1  0.03  0.05  0 0.15  -0.1  -0.1 ... 
      0.4  0.3  -0.1   0.15  -0.05  0.25  0.1  0.25  -0.3 ...
      0.3  0.15  -0.2   -0.04  0.3  0 ]'*0.1;

% next block: points at which strains are calculated
pts=[0 0 1
     0 0 -1];

% next line: compute the shape functions at the points
[N,Nx,Ny,Nz]=CompNDNatPointsHexa8(pts(:,1), pts(:,2), pts(:,3));

% next block: nodal coordinates of the element
element_nodes=[10 8 10
                20 8 10
                20 8 2
                10 8 2
                12 16 8
                16 16 8
                16 16 4
                12 16 4];

% for-loop: calculate the strains at the two points
for j=1:2
  J=CompJacobian3DatPoint(element_nodes, Nx(:,j), Ny(:,j), Nz(:,j));
  detJ=det(J);
  Jinv=inv(J);
  B=CompB6x24Hexa8atPoint(Nx(:,j), Ny(:,j), Nz(:,j), Jinv);
  ep=B*uvw
end