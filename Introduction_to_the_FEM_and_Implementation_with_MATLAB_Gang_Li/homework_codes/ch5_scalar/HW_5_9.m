clear all;

element_nodes=[1 1; -1 1; -2 -1; 2 -1];   % element nodal coordinates
[N,Nx,Ny]=CompNDNatPointsQuad4([1 0.5]', [-1 0.5]'); % compute shape functions at points

for i=1:2
  fprintf('Shape functions [N1 N2 N3 N4] at point %d: \n',i);
  N(:,i)'
  fprintf('Jacobian matrix at point %d: \n',i);
  J=CompJacobian2DatPoint(element_nodes, Nx(:,i), Ny(:,i))
  detJ=det(J);
  Jinv=inv(J);
  DN=zeros(2,4);
  DN(1,:)=Nx(:,i);
  DN(2,:)=Ny(:,i);
  dN=Jinv*DN;
  fprintf('x-derivatives of the shape functions ');
  fprintf('[dN1dx dN2dx dN3dx dN4dx] at point %d: \n',i);
  dN(1,:)
  fprintf('y-derivatives of shape functions ');
  fprintf('[dN1dy dN2dy dN3dy dN4dy] at point %d: \n',i);
  dN(2,:)  
end