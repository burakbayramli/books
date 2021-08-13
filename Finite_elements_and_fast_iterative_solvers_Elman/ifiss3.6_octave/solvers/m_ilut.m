function [x_it]=m_ilut(x_it,aparams,mparams)
%M_ILUT incomplete LU preconditioner
%   x_it = m_ilut(x_it,aparams,qparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix (not used)
%          mparams      structure defining preconditioning matrix
%   output
%          x_it         result of ilu preconditioning operation
%
%   IFISS function: DJS, HCE; 16 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

x_it=(mparams.U\(mparams.L\x_it));
