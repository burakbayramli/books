function x_it=m_st_block(x_it,mparams)
%M_ST_BLOCK block preconditioner for Stokes equations
%   x_it = m_st_block(x_it,mparams);
%   input
%          x_it         operand for preconditioning operator
%          mparams      structure defining block preconditioning operator
%   output
%          x_it         result of ilu preconditioning operation
%
%   IFISS function: DJS, HCE; 9 April 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

nv=size(mparams.Ast,1); np=size(mparams.Q,1);
rv=x_it(1:nv); rp=x_it(nv+1:nv+np);
zv=mparams.Ast\rv; zp=mparams.Q\rp;
x_it = [zv;zp];
