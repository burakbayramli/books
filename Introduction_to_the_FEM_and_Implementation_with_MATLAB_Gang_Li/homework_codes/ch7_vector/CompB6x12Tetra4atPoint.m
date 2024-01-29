% Compute strain-displacement matrix
function B= CompB6x12Tetra4atPoint(Nx_v, Ny_v, Nz_v, Jinv)
B=zeros(6,12);
dNmaster=zeros(3,1);
for i=1:3
  dNmaster(1,1)=Nx_v(i);
  dNmaster(2,1)=Ny_v(i);
  dNmaster(3,1)=Nz_v(i);
  dN=Jinv*dNmaster;
  B(1,3*i-2)=dN(1,1);
  B(4,3*i-1)=dN(1,1);
  B(6,3*i) = dN(1,1);
  B(2,3*i-1)=dN(2,1);
  B(4,3*i-2)=dN(2,1);
  B(5,3*i) = dN(2,1);
  B(3,3*i)=dN(3,1); 
  B(5,3*i-1)=dN(3,1); 
  B(6,3*i-2) = dN(3,1); 
end