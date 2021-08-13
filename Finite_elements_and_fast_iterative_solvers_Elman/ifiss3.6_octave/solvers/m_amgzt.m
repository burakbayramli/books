function [x_it]=m_amgzt(x_it,aparams,mparams)
%M_AMGZT AMG preconditioner for scalar operator
%   x_it = m_amgt(x_it,aparams,mparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      not used
%          mparams      not used
%   output
%          x_it         result of MG preconditioning operation
%
%   Information needed for preconditioning passed via global variables
%   calls function amg_v_cycle
%   IFISS function: DJS; 7 December 2009.
% Copyright (c) 2005 D.J. Silvester, J. Boyle


% action of inv(M)
global amg_grid amg_smoother
x_it=amg_v_cycle(x_it, amg_grid,  amg_smoother);
return
