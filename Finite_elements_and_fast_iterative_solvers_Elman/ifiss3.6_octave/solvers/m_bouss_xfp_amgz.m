function y = m_bouss_xfp_amgz(x_it,aparams,mparams);
%M_BOUSS_XFP_AMGZ AMG iterated modified PCD preconditioner
%   y = m_bouss_xfp_amgz(x_it,aparams,mparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%   output
%          y            result of preconditioning operation
%
%   calls function amg_v_cycle 
%   Global array variables amg_gridA, amg_gridFt and amg_gridF define the
%   data for amg_v_cycle for BBt, Ft and F respectively
%   IFISS function: DJS; 27 April 2012.
% Copyright (c) 2012 D.J. Silvester, M.L. Mihajlovic.

global  amg_gridA amg_smootherA amg_gridF amg_smootherF amg_gridFt  amg_smootherFt
nv = length(aparams.F);
np = size(aparams.B,1); 
nt = size(aparams.Mt,2);
Gdiag=spdiags(diag(mparams.G),0,nv,nv);
rv=x_it(1:nv); rp=x_it(nv+1:nv+np); rt=x_it(nv+np+1:nv+np+nt);

%% temperature zt = aparams.Ft \ rt;
zt = amg_v_cycle(rt, amg_gridFt,  amg_smootherFt);

%% pressure Poisson setup
xB = (Gdiag\aparams.B')';
BBt = aparams.B*xB'; 

%% pressure  zp = -(BBt)\((mparams.Fp)*((mparams.Mp)\rp));
zp=amg_v_cycle(-((mparams.Fp)*((mparams.Mp)\rp)), amg_gridA,  amg_smootherA);

%% velocity solve
rv = rv +(aparams.Mt)*zt - (aparams.B')*zp;
zv = amg_v_cycle(rv, amg_gridF,  amg_smootherF);
y = [zv;zp;zt];
return
