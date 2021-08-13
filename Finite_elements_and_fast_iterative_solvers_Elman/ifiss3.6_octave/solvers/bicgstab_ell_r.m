function [x,flag,iter,resvec] = bicgstab_ell_r(aparams,mparams, b, params, x0)
%BICGSTAB_ELL_R bicgstab(ell) iteration right preconditioning 
%   [x, flag,iter,resvec] = bicgstab_ell_r(aparams,mparams, b, params, x0)
%   input
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%          b            right-hand side vector
%          params       four-dimensional vector to control iteration
%            params(1) = relative residual reduction factor
%            params(2) = max number of iterations 
%            params(3), params(4):  see source file bicgstab_ell.m for definitions
%          x0           initial iterate
%   output
%          x            computed (approximate) solution
%          flag         convergence flag
%            0 for convergence to params(1) within params(2) iterations
%            1 if iterated params(2) times but did not converge to params(1)
%          iter         total iteration count
%          resvec       vector of residual norms of iterates
%
%   calls bicgstab_ell
%   IFISS function: HCE; 15 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

% Get names of mvp routine and preconditioning routine
afun = aparams.Afun;
mfun = mparams.Mfun;
%
% Compute initial residual
r0 = b - feval(afun,x0,aparams);
%
% Solve for correction for preconditioned system using BiCGSTAB(ell) with zero
% initial vector
c0_hat = zeros(length(b),1);
% Adjust tolerance so stopping test is |rk| < tol * |b|
rparams = params;
rparams(1) = rparams(1)*norm(b)/norm(r0);
[c_hat,flag,iter,resvec] = bicgstab_ell(aparams,mparams, r0, rparams,c0_hat);
%
% Apply preconditioner to computed (preconditioned) correction to get true
% correction, and then add correction to initial iterate
c = feval(mfun,c_hat,aparams,mparams);
x = x0 + c;
