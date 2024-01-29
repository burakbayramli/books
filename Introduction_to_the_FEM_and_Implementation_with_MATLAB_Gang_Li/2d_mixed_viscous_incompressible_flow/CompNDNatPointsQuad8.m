% Compute the shape functions and their first derivatives at a set
% of points defined by (xi_vector,eta_vector) in a 2-D 8-node square 
% master element. Input is a single point when the length of 
% xi _vector and eta _vector is one 
% Input: xi_vector,eta_vector: coordinates of the input points 
% Output: N: matrix storing shape function values with the format
%         [N1(xi_1,eta_1)  N1(xi_2,eta_2)  N1(xi_3,eta_3) ...  
%          N2(xi_1,eta_1)  N2(xi_2,eta_2)  N2(xi_3,eta_3) ...
%          N3(xi_1,eta_1)  N3(xi_2,eta_2)  N3(xi_3,eta_3) ...
%          ...              ...           ...             ...]
% Output: Nx, Ny: matrice of dNi/dxi(xi,eta) and dNi/deta(xi,eta)
%         respectively, format is the same as N
function [N,Nx,Ny]=CompNDNatPointsQuad8(xi_vector, eta_vector)
np=size(xi_vector,1);
N=zeros(8,np); Nx=zeros(8,np); Ny=zeros(8,np); % set up empty matrices
master_nodes=[1 1; -1 1; -1 -1; 1 -1; ... % coordinates of the nodes
              0 1; -1 0;  0 -1; 1  0];    % of the master element 
% for loop: compute N, Nx, Ny
for j=1:np                              % columns for point 1,2 ...
  xi=xi_vector(j);                      % xi-coordinate of point j 
  eta=eta_vector(j);                    % eta-coordinate of point j 
  N(5,j)=1/2*(1-xi^2)*(1+eta);
  N(6,j)=1/2*(1-xi)*(1-eta^2);
  N(7,j)=1/2*(1-xi^2)*(1-eta);
  N(8,j)=1/2*(1+xi)*(1-eta^2);
  Nx(5,j)=-xi*(1+eta);
  Nx(6,j)=-1/2*(1-eta^2);
  Nx(7,j)=-xi*(1-eta);
  Nx(8,j)=1/2*(1-eta^2);
  Ny(5,j)=1/2*(1-xi^2);
  Ny(6,j)=(1-xi)*(-eta);
  Ny(7,j)=-1/2*(1-xi^2);
  Ny(8,j)=(1+xi)*(-eta);
  for i=1:4
    nx=master_nodes(i,1); 
    ny=master_nodes(i,2);
    if i==1; jj=8; else jj=i+3; end
    k=i+4;  
    N(i,j)=(1.0 + nx*xi)*(1.0 + ny*eta)/4.0 - 1/2*(N(jj,j)+N(k,j));
    Nx(i,j)= nx*(1.0 + ny*eta)/4.0 - 1/2*(Nx(jj,j)+Nx(k,j));
    Ny(i,j)= ny*(1.0 + nx*xi)/4.0 - 1/2*(Ny(jj,j)+Ny(k,j));
  end
end