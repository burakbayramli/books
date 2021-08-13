function [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex2(xy,bound,square_type)
%POISSONCONTROL_RHS_EX2 inputs desired state and BCs for Problem 2
%Dirichlet boundary condition for Problem 2 
%   [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex2(xy,bound,square_type)
%   input
%          xy           vertex coordinate vector  
%          bound        boundary vertex vector
%          square_type  specifies whether square is [0,1]^2 or [-1,1]^2
%   output
%          yhat_vec     vector relating to desired state
%          bc_nodes     vector relating to Dirichlet boundary condition
%   
%   IFISS function: JWP; 27 June 2012.
% Copyright (c) 2012 J.W. Pearson

if square_type==1 % specify functions if square is [0,1]^2
    yhat_vec = (xy(:,1)<=0.5).*(xy(:,2)<=0.5); % specify desired state
    bc_nodes = zeros(length(bound),1); % specify Dirichlet BC
elseif square_type==2 % do same if square is [-1,1]^2
    yhat_vec = (xy(:,1)<=0).*(xy(:,2)<=0);
    bc_nodes = zeros(length(bound),1);
end
