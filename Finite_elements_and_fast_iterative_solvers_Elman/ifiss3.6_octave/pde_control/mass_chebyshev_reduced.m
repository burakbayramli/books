function x_it=mass_chebyshev_reduced(x_it,massparams)
%MASS_CHEBYSHEV_REDUCED as above for (reduced) 2x2 block system
%control problem, using Chebyshev semi-iteration
%   x_it = mass_chebyshev_reduced(x_it,mparams);
%   input
%          x_it         operand for preconditioning operator
%          mparams      structure defining block preconditioning operator
%   output
%          x_it         result of preconditioning operation
%
%   IFISS function: HCE, JWP, DJS; 20 October 2013.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage

x_it = m_masscheb(x_it,massparams);

