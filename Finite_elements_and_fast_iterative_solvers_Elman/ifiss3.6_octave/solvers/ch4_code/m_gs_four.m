function w=m_gs_four(v,aparams,mparams)
%m_gs_one     four-directional Gauss-Seidel preconditioning
%   w = m_gs_four(v,aparams,mparams)
%   input
%          v            operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%                          factored splitting operators Li*Ui, i=1,2,3,4
%   output
%          w            result of preconditioning operation
%
%   IFISS function: HCE; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

w = mparams.U1\(mparams.L1\v);
r = v - aparams.A*w;  w = w + mparams.U2\(mparams.L2\r);
r = v - aparams.A*w;  w = w + mparams.U3\(mparams.L3\r);
r = v - aparams.A*w;  w = w + mparams.U4\(mparams.L4\r);