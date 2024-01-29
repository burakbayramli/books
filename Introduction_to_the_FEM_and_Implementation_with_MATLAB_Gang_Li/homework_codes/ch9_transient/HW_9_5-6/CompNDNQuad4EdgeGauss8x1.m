%-------------------------------------------
% Compute N(x,y), dNdx(x,y), dNdy(x,y) 
% at the Gauss points on the edges 
% of a square master element
%
%       --2--1--
%      |        |
%      3        8
%      |        |
%      4        7
%      |        |
%       --5--6--
% 
%--------------------------------------------
function [N, Nx, Ny]=CompNDNQuad4EdgeGauss8x1()

  [gauss_points, gauss_weights]=GetQuadEdgeGauss8x1();
  
  N=zeros(4,8);
  Nx=zeros(4,8);
  Ny=zeros(4,8);
  
  for j=1:8
    x=gauss_points(j,1);
    y=gauss_points(j,2);
    [Np,Npx,Npy]=CompNDNatPointQuad4(x,y);
    N(:,j)=Np;
    Nx(:,j)=Npx;
    Ny(:,j)=Npy;
  end
