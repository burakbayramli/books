function y = m_sxbfbt_amgz(x_it,aparams,qparams)
%M_SXBFBT_AMGZ AMG iterated stabilized LSC preconditioner
%   y = m_sxbfbt_amgz(x_it,aparams,qparams)
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
%   IFISS function: HCE; 28 December 2009.
% Copyright (c) 2009 D.J. Silvester, J. Boyle, H. Elman

global  amg_gridA amg_smootherA amg_gridF amg_smootherF
nv = length(aparams.F);
%nu = nv/2;
np = size(aparams.B,1);

sigma = qparams.viscosity;

rv=x_it(1:nv); rp=x_it(nv+1:nv+np);

%% pressure solve
xB = (qparams.Gdiag\aparams.B')';

%qparams.domain=2; %%treat matrix as non-singular
if qparams.domain==1,
   n_null = qparams.n_null;
   minor = [1:n_null-1,n_null+1:np]';
   rp1 = zeros(np,1);
   rp1(minor) = amg_v_cycle(rp(minor), amg_gridA,  amg_smootherA);
   rp2 =  xB*(aparams.F*(xB'*rp1)) + sigma*(qparams.Cp2*rp1);
   zp = zeros(np,1);
   zp(minor) =  - amg_v_cycle(rp2(minor), amg_gridA,  amg_smootherA);
else 
   rp1 = amg_v_cycle(rp, amg_gridA,  amg_smootherA);
   rp2 = xB*(aparams.F*(xB'*rp1)) + sigma*(qparams.Cp2*rp1);
   zp  =  - amg_v_cycle(rp2, amg_gridA,  amg_smootherA);
end
%qparams.domain=1;

%% velocity solve
rv = rv-(aparams.B')*zp;
zv = amg_v_cycle(rv, amg_gridF,  amg_smootherF);
y = [zv;zp];
