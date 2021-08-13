function [x_it]=m_nonet(x_it,aparams,marams)
%M_NONET "no preconditioning" operator
%   y = m_nonet(x_it,aparams,mparams);
%   input
%          x_it        operand for preconditioning operator
%          aparams     structure defining coefficient matrix
%          marams      structure defining preconditioning matrix
%   output
%          x_it        result of (no) preconditioning operation
%
%   IFISS function: DJS; 15 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage
x_it=x_it;
