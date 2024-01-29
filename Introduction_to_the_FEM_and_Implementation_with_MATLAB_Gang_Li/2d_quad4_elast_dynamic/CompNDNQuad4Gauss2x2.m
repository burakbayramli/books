
function [N,Nx,Ny]=CompNDNQuad4Gauss2x2()
  
  [gauss_points, gauss_weights]=GetQuadGauss2x2();
  
  N=zeros(4,4);
  Nx=zeros(4,4);
  Ny=zeros(4,4);
  
  for j=1:4
    x=gauss_points(j,1);
    y=gauss_points(j,2);
    [Np,Npx,Npy]=CompNDNatPointQuad4(x,y);
    N(:,j)=Np;
    Nx(:,j)=Npx;
    Ny(:,j)=Npy;
  end
