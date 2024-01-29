% Compute the shape functions and their first derivatives at a set
% of points defined by (xi_vec, eta_vec, zeta_vec) in a 3-D 
% 4-node tetrahedral master element. 
% Input is a single point when the length of the input vectors is 1
% Input: xi_vec, eta_vec, zeta_vec: coordinates of the input points 
% Output: N: matrix storing shape function values with the format
%         [N1(xi_1, eta_1, zeta_1)  N1(xi_2, eta_2, zeta_2) ...  
%          N2(xi_1, eta_1, zeta_1)  N2(xi_2, eta_2, zeta_2) ... 
%          N3(xi_1, eta_1, zeta_1)  N3(xi_2, eta_2, zeta_2) ... 
%          ...              ...           ...             ...]
% Output: Nx, Ny, Nz: matrice of dNi/dxi(xi, eta, zeta), 
%         dNi/deta(xi, eta, zeta), dNi/dzeta(xi, eta, zeta), 
%         respectively, format is the same as N
function [N,Nx,Ny,Nz]=CompNDNatPointsTetra4(xi_vec,eta_vec,zeta_vec)
np=size(xi_vec,1);
N=zeros(4,np);  Nx=zeros(4,np); 
Ny=zeros(4,np); Nz=zeros(4,np); % set up empty matrices

% for loop: compute N, Nx, Ny, Nz
for j=1:np                 % columns for point 1,2 ...
  xi=xi_vec(j);            % xi-coordinate of point j 
  eta=eta_vec(j);          % eta-coordinate of point j
  zeta=zeta_vec(j);        % zeta-coordinate of point j 
  N(1,j)=1.0-xi-eta-zeta;  % N1(xi,eta,zeta)
  N(2,j)=xi;               % N2(xi,eta,zeta)
  N(3,j)=eta;              % N3(xi,eta,zeta)
  N(4,j)=zeta;             % N4(xi,eta,zeta)
  Nx(1,j)=-1.0;            % dN1/dxi(xi,eta,zeta)          
  Nx(2,j)=1.0;             % dN2/dxi(xi,eta,zeta)        
  Ny(1,j)=-1.0;            % dN1/deta(xi,eta,zeta) 
  Ny(3,j)=1.0;             % dN3/deta(xi,eta,zeta) 
  Nz(1,j)=-1.0;            % dN1/dzeta(xi,eta,zeta)
  Nz(4,j)=1.0;             % dN4/dzeta(xi,eta,zeta)
end