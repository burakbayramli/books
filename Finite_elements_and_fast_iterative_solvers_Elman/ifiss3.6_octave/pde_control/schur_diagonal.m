function x_it=schur_diagonal(x_it,schurparams)
%SCHUR_DIAGONAL applies Schur comp. preconditioner (diagonal approx.)
%diagonal matrix inversion
%   x_it = schur_diagonal(x_it,mparams);
%   input
%          x_it         operand for preconditioning operator
%          mparams      structure defining block preconditioning operator
%   output
%          x_it         result of preconditioning operation
%
%   IFISS function: JWP, DJS, HCE; 29 June 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage

x_it = diag(diag(schurparams.D))\x_it;
