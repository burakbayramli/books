function x_it=schur_gmg(x_it,schurparams)
%SCHUR_GMG applies Schur complement preconditioner (GMG)
%   x_it = schur_gmg(x_it,mparams);
%   input
%          x_it         operand for preconditioning operator
%          mparams      structure defining block preconditioning operator
%   output
%          x_it         result of preconditioning operation
%
%   IFISS function: JWP, DJS, HCE; 29 June 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, H.C. Elman, A. Ramage

x1 = mg_iter(schurparams.mgdata,zeros(size(x_it)),x_it,schurparams.smooth_data,...
	schurparams.nc,schurparams.npre,schurparams.npost,schurparams.sweeps);
for j = 2:schurparams.no_gmg
    x1 = mg_iter(schurparams.mgdata,x1,x_it,schurparams.smooth_data,...
        schurparams.nc,schurparams.npre,schurparams.npost,schurparams.sweeps);
end
x2 = schurparams.M*x1;
x_it = mg_iter(schurparams.mgdata,zeros(size(x_it)),x2,schurparams.smooth_data,...
	schurparams.nc,schurparams.npre,schurparams.npost,schurparams.sweeps);
for j = 2:schurparams.no_gmg
    x_it = mg_iter(schurparams.mgdata,x_it,x2,schurparams.smooth_data,...
        schurparams.nc,schurparams.npre,schurparams.npost,schurparams.sweeps);
end
