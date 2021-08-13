function [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex4(xy,bound,ell_type)
%POISSONCONTROL_RHS_EX4 inputs desired state and BCs for Problem 4
%Dirichlet boundary condition for Problem 4 
%   [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex4(xy,bound,square_type)
%   input
%          xy           vertex coordinate vector  
%          bound        boundary vertex vector
%          ell_type     whether L-shape is based around [0,1] or [-1,1]
%   output
%          yhat_vec     vector relating to desired state
%          bc_nodes     vector relating to Dirichlet boundary condition
%   
%   IFISS function: JWP; 28 June 2012.
% Copyright (c) 2012 J.W. Pearson

% Specify coordinates of centre of Gaussian
if ell_type==1 % if 'L' shape is based on [0,1]
    gauss_ctr = [0.5 0.5];
%     gauss_ctr = [0.25 0.75]; % centre of target Gaussian can be altered
elseif ell_type==2 % and if 'L' shape is based on [-1,1]
    gauss_ctr = [0 0];
%     gauss_ctr = [-0.5 0.5]; % centre of target Gaussian can be altered
end

% Specify Gaussian parameter
gauss_param = 1;

% Specify desired state
yhat_vec = exp(-gauss_param*((xy(:,1)-gauss_ctr(1)).^2+ ...
    (xy(:,2)-gauss_ctr(2)).^2));
% Specify Dirichlet BC
bc_nodes = exp(-gauss_param*((xy(bound,1)-gauss_ctr(1)).^2+ ...
    (xy(bound,2)-gauss_ctr(2)).^2));
% bc_nodes = zeros(length(bound),1); % can also force BCs to be 0
