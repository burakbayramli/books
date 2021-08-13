function y = m_fp_amgz(x_it,aparams,qparams)
%M_FP_AMGZ AMG iterated PCD preconditioner
%   y = m_fp_amgz(x_it,aparams,qparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          qparams      structure defining preconditioning matrix
%   output
%          y            result of preconditioning operation
%
%   calls function amg_v_cycle 
%   Global array variables amg_gridA and amg_gridF define the
%   data for amg_v_cycle for Ap and F respectively
%   IFISS function: DJS; 7 March 2006, 6 July 2013
% Copyright (c) 2005 D.J. Silvester, J. Boyle

global  amg_gridA amg_smootherA amg_gridF amg_smootherF

nv = length(aparams.F);
nu = nv/2;
np = size(aparams.B,1);

rv=x_it(1:nv); rp=x_it(nv+1:nv+np);

%% pressure solve
if qparams.domain==1,
   n_null = qparams.n_null;
   minor = [1:n_null-1,n_null+1:np]';
   yp = zeros(np,1);
   yp(minor) =  amg_v_cycle(rp(minor), amg_gridA,  amg_smootherA);
   zp = -diag(qparams.Mp).\(qparams.Fp*yp);
else
   zp = -diag(qparams.Mp).\((qparams.Fp)*(amg_v_cycle(rp, amg_gridA,  amg_smootherA)));
end     

%% velocity solve
rv = rv-(aparams.B')*zp;
zv = amg_v_cycle(rv, amg_gridF,  amg_smootherF);
y = [zv;zp];
