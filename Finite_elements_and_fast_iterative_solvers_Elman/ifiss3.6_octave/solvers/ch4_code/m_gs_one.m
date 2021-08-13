function w=m_gs_one(v,aparams,mparams)
%m_gs_one     one-directional Gauss-Seidel preconditioning
%   w = m_gs_one(v,aparams,mparams)
%   input
%          v            operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%                          factored splitting operator L*U
%   output
%          w            result of preconditioning operation
%
%   IFISS function: HCE; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

w = mparams.U\(mparams.L\v);