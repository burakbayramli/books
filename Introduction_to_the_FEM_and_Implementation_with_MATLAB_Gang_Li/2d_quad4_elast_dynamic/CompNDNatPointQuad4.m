
function [N,Nx,Ny]=CompNDNatPointQuad4(x,y)
  
  N=zeros(4,1);
  Nx=zeros(4,1);
  Ny=zeros(4,1);

  master_nodes=[1 1; -1 1; -1 -1; 1 -1];
  
  for i=1:4
    nx=master_nodes(i,1); 
    ny=master_nodes(i,2);
    N(i)=(1.0 + nx*x)*(1.0 + ny*y)/4.0;
    Nx(i)= nx*(1.0 + ny*y)/4.0;
    Ny(i)= ny*(1.0 + nx*x)/4.0;
  end
