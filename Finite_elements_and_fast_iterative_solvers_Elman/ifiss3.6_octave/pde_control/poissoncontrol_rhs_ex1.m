function [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex1(xy,bound,square_type)
%POISSONCONTROL_RHS_EX1 inputs desired state and BCs for Problem 1
%Dirichlet boundary condition for Problem 1 
%   [yhat_vec,bc_nodes] = poissoncontrol_rhs_ex1(xy,bound,square_type)
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

if square_type==1  % specify functions if square is [0,1]^2
    yhat_vec = (xy(:,1)<=0.5).*(xy(:,2)<=0.5).* ...
        ((2*xy(:,1)-1).^2).*((2*xy(:,2)-1).^2); % specify desired state
    bc_nodes = (xy(bound,1)<=0.5).*(xy(bound,2)<=0.5).* ...
        ((2*xy(bound,1)-1).^2).*((2*xy(bound,2)-1).^2); % and Dirichlet BC
elseif square_type==2 % do same if square is [-1,1]^2
    yhat_vec = (xy(:,1)<=0).*(xy(:,2)<=0).* ...
        (xy(:,1).^2).*(xy(:,2).^2);
    bc_nodes = (xy(bound,1)<=0).*(xy(bound,2)<=0).* ...
        (xy(bound,1).^2).*(xy(bound,2).^2);
end
