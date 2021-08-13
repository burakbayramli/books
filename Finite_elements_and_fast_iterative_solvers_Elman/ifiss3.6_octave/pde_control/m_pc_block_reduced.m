function x_it=m_pc_block_reduced(x_it,mparams)
%M_ST_BLOCK block preconditioner for the 2x2 Poisson control matrix system
%   x_it = m_st_block(x_it,mparams);
%   input
%          x_it         operand for preconditioning operator
%          mparams      structure defining block preconditioning operator
%   output
%          x_it         result of ilu preconditioning operation
%
%   IFISS function: JWP, DJS, HCE; 29 June 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage

nm=size(mparams.M,1);
r1=x_it(1:nm); r2=x_it(nm+1:end);
z1=mparams.M\r1; z2=mparams.L\(mparams.M*(mparams.L\r2));
x_it = [z1;z2];