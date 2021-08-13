function x_it = m_diagt(x_it,aparams,mparams)
%M_DIAGT action of diagonal preconditioning operator
%   x_it = m_diagt(x_it,aparams,mfun_par);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix (not used)
%          mparams      structure defining preconditioning matrix
%   output
%          x_it         result of diagonal preconditioning operation
%
%   IFISS function: HCE; 16 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

x_it=(mparams.D)\x_it;
