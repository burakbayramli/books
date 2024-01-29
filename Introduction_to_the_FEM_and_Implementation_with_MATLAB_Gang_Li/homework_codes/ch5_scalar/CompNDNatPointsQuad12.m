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
function [N,Nx,Ny]=CompNDNatPointsQuad12(xi_vector, eta_vector)
np=size(xi_vector,1);
N=zeros(12,np); Nx=zeros(12,np); Ny=zeros(12,np); % set up empty matrices
master_nodes=[1 1; -1 1; -1 -1; 1 -1; ... % coordinates of the nodes
              1/3 1; -1/3 1;  -1 1/3; -1 -1/3;...
            -1/3 -1; 1/3 -1; 1 -1/3; 1 1/3];  % of the master element 
% for loop: compute N, Nx, Ny
for j=1:np                              % columns for point 1,2 ...
  xi=xi_vector(j);                      % xi-coordinate of point j 
  eta=eta_vector(j);                    % eta-coordinate of point j 

  N(1,j)=(1+xi)*(1+eta)*(-10 + 9*(xi^2 + eta^2))/32;
  Nx(1,j)=(1+eta)*(-10 + 9*(xi^2 + eta^2))/32 + (1+xi)*(1+eta)*9*xi/16;
  Ny(1,j)=(1+xi)*(-10 + 9*(xi^2 + eta^2))/32 + (1+xi)*(1+eta)*9*eta/16;

  N(2,j)=(1-xi)*(1+eta)*(-10 + 9*(xi^2 + eta^2))/32;
  Nx(2,j)=-(1+eta)*(-10 + 9*(xi^2 + eta^2))/32 + (1-xi)*(1+eta)*9*xi/16;
  Ny(2,j)=(1-xi)*(-10 + 9*(xi^2 + eta^2))/32 + (1-xi)*(1+eta)*9*eta/16;

  N(3,j)=(1-xi)*(1-eta)*(-10 + 9*(xi^2 + eta^2))/32;
  Nx(3,j)=-(1-eta)*(-10 + 9*(xi^2 + eta^2))/32 + (1-xi)*(1-eta)*9*xi/16;
  Ny(3,j)=-(1-xi)*(-10 + 9*(xi^2 + eta^2))/32 + (1-xi)*(1-eta)*9*eta/16;
 
  N(4,j)=(1+xi)*(1-eta)*(-10 + 9*(xi^2 + eta^2))/32;
  Nx(4,j)=(1-eta)*(-10 + 9*(xi^2 + eta^2))/32 + (1+xi)*(1-eta)*9*xi/16;
  Ny(4,j)=-(1+xi)*(-10 + 9*(xi^2 + eta^2))/32 + (1+xi)*(1-eta)*9*eta/16;
 
  N(5,j)=9/32*(1+eta)*(1-xi^2)*(1+3*xi);
  Nx(5,j)=9/32*(1+eta)*(-2*xi)*(1+3*xi) + 9/32*(1+eta)*(1-xi^2)*3;
  Ny(5,j)=9/32*(1-xi^2)*(1+3*xi);
  
  N(6,j)=9/32*(1+eta)*(1-xi^2)*(1-3*xi);
  Nx(6,j)=9/32*(1+eta)*(-2*xi)*(1-3*xi) + 9/32*(1+eta)*(1-xi^2)*(-3);
  Ny(6,j)=9/32*(1-xi^2)*(1-3*xi);
  
  N(7,j)=9/32*(1-xi)*(1-eta^2)*(1+3*eta);
  Nx(7,j)=-9/32*(1-eta^2)*(1+3*eta);
  Ny(7,j)=9/32*(1-xi)*(-2*eta)*(1+3*eta) + 9/32*(1-xi)*(1-eta^2)*3;

  N(8,j)=9/32*(1-xi)*(1-eta^2)*(1-3*eta);
  Nx(8,j)=-9/32*(1-eta^2)*(1-3*eta);
  Ny(8,j)=9/32*(1-xi)*(-2*eta)*(1-3*eta) + 9/32*(1-xi)*(1-eta^2)*(-3);

  N(9,j)=9/32*(1-eta)*(1-xi^2)*(1-3*xi);
  Nx(9,j)=9/32*(1-eta)*(-2*xi)*(1-3*xi) + 9/32*(1-eta)*(1-xi^2)*(-3);
  Ny(9,j)=-9/32*(1-xi^2)*(1-3*xi);

  N(10,j)=9/32*(1-eta)*(1-xi^2)*(1+3*xi);
  Nx(10,j)=9/32*(1-eta)*(-2*xi)*(1+3*xi) + 9/32*(1-eta)*(1-xi^2)*3;
  Ny(10,j)=-9/32*(1-xi^2)*(1+3*xi);

  N(11,j)=9/32*(1+xi)*(1-eta^2)*(1-3*eta);
  Nx(11,j)=9/32*(1-eta^2)*(1-3*eta);
  Ny(11,j)=9/32*(1+xi)*(-2*eta)*(1-3*eta) + 9/32*(1+xi)*(1-eta^2)*(-3);

  N(12,j)=9/32*(1+xi)*(1-eta^2)*(1+3*eta);
  Nx(12,j)=9/32*(1-eta^2)*(1+3*eta);
  Ny(12,j)=9/32*(1+xi)*(-2*eta)*(1+3*eta) + 9/32*(1+xi)*(1-eta^2)*(3);

   
end