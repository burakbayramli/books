function [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex3(xy,bound)
%POISSONCONTROL_RHS_EX3 inputs desired state and BCs for Problem 3
%Dirichlet boundary condition for Problem 3 
%   [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex3(xy,bound,square_type)
%   input
%          xy           vertex coordinate vector  
%          bound        boundary vertex vector
%   output
%          yhat_vec     vector relating to desired state
%          bc_nodes     vector relating to Dirichlet boundary condition
%   
%   IFISS function: JWP; 27 June 2012.
% Copyright (c) 2012 J.W. Pearson

% Specify desired state
yhat_vec = sin(pi*xy(:,1)).*sin(pi*xy(:,2));
% Specify Dirichlet BC
bc_nodes = zeros(length(bound),1);
