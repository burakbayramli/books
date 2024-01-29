% Compute strain-displacement matrix
function B= CompB6x24Hexa8atPoint(Nx_v, Ny_v, Nz_v, Jinv)
B=zeros(6,24);
dNmaster=zeros(3,1);
for i=1:8
  dNmaster=[Nx_v(i); Ny_v(i); Nz_v(i)];
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
