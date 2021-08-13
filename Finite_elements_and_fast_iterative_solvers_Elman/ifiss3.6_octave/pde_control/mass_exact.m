function x_it=mass_exact(x_it,massparams)
%MASS_EXACT applies (1,1)-block preconditioner (exact solution)
%mass matrix inversion
%   x_it = mass_exact(x_it,mparams);
%   input
%          x_it         operand for preconditioning operator
%          mparams      structure defining block preconditioning operator
%   output
%          x_it         result of preconditioning operation
%
%   IFISS function: JWP, DJS, HCE; 29 June 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage

nm = size(massparams.M,1);
r1 = x_it(1:nm); r2 = x_it(nm+1:2*nm);
z1 = massparams.M\r1; z2 = 1/massparams.beta*(massparams.M\r2);
x_it = [z1; z2];
