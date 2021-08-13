function x_it=schur_exact(x_it,schurparams)
%SCHUR_EXACT applies Schur complement preconditioner (exact solution)
%exact matrix solves
%   x_it = schur_exact(x_it,mparams);
%   input
%          x_it         operand for preconditioning operator
%          mparams      structure defining block preconditioning operator
%   output
%          x_it         result of preconditioning operation
%
%   IFISS function: JWP, DJS, HCE; 29 June 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage

x_it = schurparams.L\(schurparams.M*(schurparams.L\x_it));
