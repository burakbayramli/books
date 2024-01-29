% Compute the shape functions and their first derivatives at a set
% of points defined by (xi_vector,eta_vector) in a 2-D 4-node square 
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
function [N,Nx,Ny]=CompNDNatPointsQuad4(xi_vector, eta_vector)
np=size(xi_vector,1);
N=zeros(4,np); Nx=zeros(4,np); Ny=zeros(4,np); % set up empty matrices
master_nodes=[1 1; -1 1; -1 -1; 1 -1];  % coordinates of the nodes
                                        % of the master element 
% for loop: compute N, Nx, Ny
for j=1:np                              % columns for point 1,2 ...
  xi=xi_vector(j);                      % xi-coordinate of point j 
  eta=eta_vector(j);                    % eta-coordinate of point j 
  for i=1:4                             % rows for N1, N2, ...
    nx=master_nodes(i,1);                 
    ny=master_nodes(i,2);
    N(i,j)=(1.0 + nx*xi)*(1.0 + ny*eta)/4.0;   % Ni(xi,eta)
    Nx(i,j)= nx*(1.0 + ny*eta)/4.0;            % dNi/dxi(xi,eta)
    Ny(i,j)= ny*(1.0 + nx*xi )/4.0;            % dNi/deta(xi,eta)
  end
end