function x_it=mass_chebyshev(x_it,massparams)
%MASS_CHEBYSHEV applies (1,1)-block preconditioner (Chebyshev)
%Chebyshev semi-iteration
%   x_it = mass_chebyshev(x_it,mparams);
%   input
%          x_it         operand for preconditioning operator
%          mparams      structure defining block preconditioning operator
%   output
%          x_it         result of preconditioning operation
%
%   IFISS function: DJS, JWP, HCE; 28 August 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage

nm = size(massparams.Q,1);
r1 = x_it(1:nm); r2 = x_it(nm+1:2*nm);
z1 = m_masscheb(r1,massparams);
%m_masscheb(r1,massparams.Q,massparams.its,massparams.qmethod);
z2 = (1/massparams.beta)*m_masscheb(r2,massparams);
%     *m_masscheb(r2,massparams.Q,massparams.its,massparams.qmethod);
x_it = [z1; z2];
