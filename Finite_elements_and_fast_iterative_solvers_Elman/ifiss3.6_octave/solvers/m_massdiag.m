function x_it = m_massdiag(x_it,qparams)
%M_MASSDIAG mass matrix diagonal preconditioning operator
%   x_it = m_massdiag(x_it,qparams);
%   input
%          x_it       operand for preconditioning operator
%     qparams.Q       mass matrix
%   output
%          x_it       result of preconditioning operation
%
%   IFISS function: DJS; 2 January 2011.
% Copyright (c) 2010 D.J. Silvester, V. Simoncini
Mdiag=diag(qparams.Q); 
x_it=x_it./Mdiag;
