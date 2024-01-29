
%------------------------------------
% Compute strain-displacement matrix
%------------------------------------
function B= CompB4x8Quad4Gauss2x2(g, Nx, Ny)

  B=zeros(4,8);
  
  for i=1:4
    B(1,2*i-1)=Nx(i,g);
    B(3,2*i)= Nx(i,g);
    B(2,2*i-1)=Ny(i,g);
    B(4,2*i)= Ny(i,g);
  end
