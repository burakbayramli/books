% Compute the shape functions and their derivatives at points defined
% by (xi_vec,eta_vec) in a 2-D 6-node triangular master element with 
% master_nodes=[0 0; 1 0; 0 1; 0.5 0; 0.5 0.5; 0 0.5];
% Input: xi_vec,eta_vec: coordinates of the input points 
% Output: N: matrix storing shape function values with the format
%         [N1(xi_1,eta_1)  N1(xi_2,eta_2)  N1(xi_3,eta_3) ...  
%          N2(xi_1,eta_1)  N2(xi_2,eta_2)  N2(xi_3,eta_3) ...
%          N3(xi_1,eta_1)  N3(xi_2,eta_2)  N3(xi_3,eta_3) ...
%          ...              ...           ...             ...]
% Output: Nx, Ny: matrice of dNi/dxi(xi,eta) and dNi/deta(xi,eta)
%         respectively, format is the same as N
function [N,Nx,Ny]=CompNDNatPointsTri6(xi_vec, eta_vec)
np=size(xi_vec,1);  % number of points
N=zeros(6,np);  Nx=zeros(6,np);  Ny=zeros(6,np); % empty matrices
% for loop: compute N, Nx, Ny
for j=1:np                           % columns for point 1,2 ...
  xi=xi_vec(j);                      % xi-coordinate of point j 
  eta=eta_vec(j);                    % eta-coordinate of point j 
  % next 3 lines: N1(xi,eta), dN1/dxi(xi, eta), dN1/deta(xi, eta) 
  N(1,j)= 2*(1-xi-eta)*(1-xi-eta -0.5);
  Nx(1,j)= -2*(1-xi-eta - 0.5)  -2*(1-xi-eta);
  Ny(1,j)=-2*(1-xi-eta  - 0.5) -2*(1-xi-eta);
  % next 3 lines: N2 and associated derivatives
  N(2,j)= 2*xi*(xi - 0.5);
  Nx(2,j)= 2*(xi - 0.5) + 2*xi;
  Ny(2,j)= 0;
  % next 3 lines: N3 and associated derivatives
  N(3,j)= 2*eta*(eta - 0.5);
  Nx(3,j)= 0;
  Ny(3,j)= 2*(eta - 0.5) + 2*eta;
  % next 3 lines: N4 and associated derivatives
  N(4,j)= 4*(1-xi-eta)*xi;
  Nx(4,j)= -4*xi + 4*(1-xi-eta);
  Ny(4,j)= -4*xi;
  % next 3 lines: N5 and associated derivatives
  N(5,j)= 4*xi*eta;
  Nx(5,j)= 4*eta;
  Ny(5,j)= 4*xi;
  % next 3 lines: N6 and associated derivatives
  N(6,j)= 4*eta*(1-xi-eta);
  Nx(6,j)= -4*eta;
  Ny(6,j)= 4*(1-xi-eta) -4*eta;
end