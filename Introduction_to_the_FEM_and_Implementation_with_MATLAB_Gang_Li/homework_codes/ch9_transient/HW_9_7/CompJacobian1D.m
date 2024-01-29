% Compute the Jacobian of an isoparametric element mapping
% Input: x_vector: vector containing x-coordinates of nodes
% Input: Nx, shape function derivatives, from "CompShapeLinear1D"
% Output: J, Jacobian vector J(xi_1, xi_2, ...)^T
function J=CompJacobian1D(x_vector,Nx)
  J=(x_vector'*Nx)';   % compute vector of Jacobian      