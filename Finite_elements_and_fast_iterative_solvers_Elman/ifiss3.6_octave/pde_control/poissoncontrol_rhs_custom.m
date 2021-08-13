function [yhat_vec,bc_nodes] = ...
    poissoncontrol_rhs_custom(xy,bound,square_type)
%POISSONCONTROL_RHS_CUSTOM inputs specific desired state and BCs
%desired state and Dirichlet boundary condition for a chosen test problem
%   [yhat_vec,bc_nodes] = poissoncontrol_rhs_custom(xy,bound,square_type)
%   input
%          xy           vertex coordinate vector  
%          bound        boundary vertex vector
%          square_type  specifies whether square is [0,1]^2 or [-1,1]^2
%   output
%          yhat_vec     vector relating to desired state
%          bc_nodes     vector relating to Dirichlet boundary condition
%
% Guide for user:
%   For square_type=1 and square_type=2, specify vectors yhat_vec and
%   bc_nodes. For example:
%     - If desired state is given by x_1*x_2, then type
%          yhat_vec = xy(:,1).*xy(:,2);
%     - If desired state is given by 1 if x_2<=0.5 and 0 o.w., then type:
%          yhat_vec = xy(:,2)<=0.5;
%     - If Dirichlet BC is given by x_1+x_2, then type:
%          bc_nodes = xy(bound,1)+xy(bound,2);
%     - If Dirichlet BC is 1 on top and right boundaries and 0 o.w., type:
%          bc_nodes = (xy(bound,2)==1)+(xy(bound,2)==1)- ...
%             ((xy(bound,2)==1).*(xy(bound,2)==1));
%       The final term is to ensure the conditions x_1=0 and x_2=0 aren't
%       "counted twice" at node (1,1); without it BC would be 2 there.
%   
%   IFISS function: JWP; 27 June 2012.
% Copyright (c) 2012 J.W. Pearson

if square_type==1 % specify functions if square is [0,1]^2
    yhat_vec = ones(length(xy),1);
    bc_nodes = zeros(length(bound),1);
elseif square_type==2 % do same if square is [-1,1]^2
    yhat_vec = ones(length(xy),1);
    bc_nodes = zeros(length(bound),1);
end
