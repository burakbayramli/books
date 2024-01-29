% Function to compute Jacobian of an isoparametric
% element mapping
% Output: Jacobian vector J(xi_1, xi_2, ...)^T

function [J]=compIsoJacobian1D(node_x_vector,Nx)

  n=size(Nx,1);
  nxi=size(Nx,2);
  J=zeros(nxi,1);
  for i=1:nxi
    for j=1:n
      J(i)=J(i)+Nx(j,i)*node_x_vector(j);
     end
  end