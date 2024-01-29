% Compute the shape functions and their first derivatives at a set
% of points defined by (xi_vector,eta_vector) in a 2-D 3-node square 
% master element. Input is a single point when the length of 
% xi _vector and eta _vector is one 
% Input: xi_vector,eta_vector: coordinates of the input points 
% Output: N: matrix storing shape function values with the format
%         [N1(xi_1,eta_1)  N1(xi_2,eta_2)  N1(xi_3,eta_3) ...  
%          N2(xi_1,eta_1)  N2(xi_2,eta_2)  N2(xi_3,eta_3) ...
%          N3(xi_1,eta_1)  N3(xi_2,eta_2)  N3(xi_3,eta_3) ...]
% Output: Nx, Ny: matrice of dNi/dxi(xi,eta) and dNi/deta(xi,eta)
%         respectively, format is the same as N
function [N,Nx,Ny]=CompNDNatPointsTri3(xi_vector, eta_vector)
np=size(xi_vector,1);
N=zeros(3,np); Nx=zeros(3,np); Ny=zeros(3,np); % set up empty matrices
master_nodes=[0 0; 1 0; 0 1];           % coordinates of the nodes
                                        % of the master element 
% for loop: compute N, Nx, Ny
for j=1:np                              % columns for point 1,2 ...
  xi=xi_vector(j);                      % xi-coordinate of point j 
  eta=eta_vector(j);                    % eta-coordinate of point j 
  N(1,j)=1.0 - xi - eta; 
  N(2,j)=xi; 
  N(3,j)=eta;
  Nx(1,j)=-1.0; 
  Nx(2,j)=1.0; 
  Ny(1,j)=-1.0; 
  Ny(3,j)=1.0; 
end