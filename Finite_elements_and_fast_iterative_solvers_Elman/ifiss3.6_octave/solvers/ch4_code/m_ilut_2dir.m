function w = m_ilut_2dir(v,aparams,mparams)
%m_ilut_2dir     two-directional ILU preconditioning
%   w = m_ilut_2dir(v,aparams,mparams)
%   input
%          v            operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%                          factored splitting operators L*U, Lv*Uv
%                          permutation vectors p, ip
%   output
%          w            result of preconditioning operation
%
%   IFISS function: HCE; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

w = mparams.U\(mparams.L\v); 
r = v - aparams.A*w;  
rp = r(mparams.ip);
rp = mparams.Uv\(mparams.Lv\rp);
rpp = rp(mparams.p);
w = w + rpp;