function [x_it]=m_amgzz(f,aparams)
%M_AMGZZ AMG preconditioner with multiple v-cycles
%   x_it = m_amgzz(x_it,aparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure for matrix vector multiply
%   output
%          x_it         result of MG preconditioning operation
%
%   Information needed for preconditioning passed via global variables
%   calls function amg_v_cycle
%   IFISS function: DJS; 2 January 2011.
% Copyright (c) 2005 D.J. Silvester, J. Boyle
global amg_grid amg_smoother number_vcycles
% 
r=f;   rn0=norm(r);         % initial residual
x_it = zeros(length(r),1);  % intial vector
for k=1:number_vcycles
x_it= x_it + amg_v_cycle(r, amg_grid,  amg_smoother);
r=f-aparams.A*x_it;rn=norm(r);
%fprintf('%5i %16.4e %16.4f \n', k, rn, log10(rn/rn0));
end
return
