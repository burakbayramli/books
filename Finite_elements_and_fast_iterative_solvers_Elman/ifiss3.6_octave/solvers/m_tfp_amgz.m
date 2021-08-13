function y = m_tfp_amgz(x_it,aparams,mparams);
%M_TFP_AMGZ AMG iterated modified PCD preconditioner
%   y = m_tfp_amgz(x_it,aparams,mparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%   output
%          y            result of preconditioning operation
%
%   calls function amg_v_cycle 
%   Global array variables amg_gridA and amg_gridF define the
%   data for amg_v_cycle for BBt and F respectively
%   IFISS function: DJS; 19 July 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

global  amg_gridA amg_smootherA amg_gridF amg_smootherF
nv = length(aparams.F);
nu = nv/2;
np = size(aparams.B,1);


rv=x_it(1:nv); rp=x_it(nv+1:nv+np);

%% pressure Poisson setup
Gdiag=spdiags(diag(mparams.G),0,nv,nv);
xB = (Gdiag\aparams.B')';
BBt = aparams.B*xB'; 

%% pressure  zp = -(BBt)\((mparams.Fp)*((mparams.Mp)\rp));
zp=amg_v_cycle(-((mparams.Fp)*(diag(mparams.Mp).\rp)), amg_gridA,  amg_smootherA);

%% velocity solve
rv = rv-(aparams.B')*zp;
zv = amg_v_cycle(rv, amg_gridF,  amg_smootherF);
y = [zv;zp];
return
