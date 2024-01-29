% Compute the shape functions and their first derivatives at a set
% of points defined by (xi_v, eta_v, zeta_v) in a 3-D 
% 8-node hexahedral master element. 
% Input: xi_vec, eta_vec, zeta_vec: coordinates of the input points 
% Output: N: matrix storing shape function values with the format
%         [N1(xi_1, eta_1, zeta_1)  N1(xi_2, eta_2, zeta_2) ...  
%          N2(xi_1, eta_1, zeta_1)  N2(xi_2, eta_2, zeta_2) ... 
%          N3(xi_1, eta_1, zeta_1)  N3(xi_2, eta_2, zeta_2) ... 
%          ...              ...           ...             ...]
% Output: Nx, Ny, Nz: matrice of dNi/dxi(xi, eta, zeta), 
%         dNi/deta(xi, eta, zeta), dNi/dzeta(xi, eta, zeta), 
%         respectively, format is the same as N
function [N,Nx,Ny,Nz]=CompNDNatPointsHexa8(xi_v, eta_v, zeta_v)
np=size(xi_v,1);
N=zeros(8,np);  Nx=zeros(8,np); 
Ny=zeros(8,np); Nz=zeros(8,np); % set up empty matrices

master_nodes=[-1 -1 -1; 1 -1 -1; 1 1 -1; -1 1 -1;...
              -1 -1 1; 1 -1 1; 1 1 1; -1 1 1];
% for loop: compute N, Nx, Ny
for j=1:np                         % columns for point 1,2 ...
  xi=xi_v(j);                      % xi-coordinate of point j 
  eta=eta_v(j);                    % eta-coordinate of point j
  zeta=zeta_v(j);                  % zeta-coordinate of point j
  for i=1:8
    nx=master_nodes(i,1); 
    ny=master_nodes(i,2);
    nz=master_nodes(i,3);
    N(i,j)=(1.0 + nx*xi)*(1.0 + ny*eta)*(1.0 + nz*zeta)/8.0;
    Nx(i,j)= nx*(1.0 + ny*eta)*(1.0 + nz*zeta)/8.0;
    Ny(i,j)= ny *(1.0 + nx*xi)*(1.0 + nz*zeta)/8.0;
    Nz(i,j)= nz *(1.0 + nx*xi)*(1.0 + ny*eta)/8.0;
  end
end