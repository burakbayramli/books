function x_it=schur_amg(x_it,schurparams)
%SCHUR_AMG applies Schur complement preconditioner (AMG)
%   x_it = schur_amg(x_it,mparams);
%   input
%          x_it         operand for preconditioning operator
%          mparams      structure defining block preconditioning operator
%   output
%          x_it         result of preconditioning operation
%
%   IFISS function: JWP, DJS, HCE; 29 June 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage

x1 = m_amgzz(x_it,schurparams);
x2 = schurparams.M*x1;
x_it = m_amgzz(x2,schurparams);
