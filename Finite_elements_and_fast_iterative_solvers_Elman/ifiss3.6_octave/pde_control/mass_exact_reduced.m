function x_it=mass_exact_reduced(x_it,massparams)
%MASS_EXACT_REDUCED as above for (reduced) 2x2 block system
%control problem, using exact mass matrix inversion
%   x_it = mass_exact_reduced(x_it,mparams);
%   input
%          x_it         operand for preconditioning operator
%          mparams      structure defining block preconditioning operator
%   output
%          x_it         result of preconditioning operation
%
%   IFISS function: JWP, DJS, HCE; 29 June 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage

x_it = massparams.M\x_it;
