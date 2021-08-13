function y = m_sxbfbt_bc_amgz(x_it,aparams,qparams)
%M_SXBFBT_BC_AMGZ AMG iterated boundary-adjusted LSC preconditioner
%   y = m_xbfbt_bc_amgz(x_it,aparams,qparams)
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix (F, B)
%          qparams      structure defining preconditioning operator
%                       (domain, Gdiag=diagonal of velocity mass matrix, BCWeights)
%   output
%          y            result of preconditioning operation
%
%   calls function amg_v_cycle 
%   Global array variables amg_gridA, amg_gridAw and amg_gridF define the
%   data for amg_v_cycle for Ap, weighted Ap and F respectively
%   IFISS function: HCE; 22 August 2012.
% Copyright (c) 2005 D. J. Silvester, J. Boyle and H. C. Elman

global  amg_gridA amg_smootherA amg_gridAw amg_smootherAw amg_gridF amg_smootherF
nv = length(aparams.F);
%nu = nv/2;
np = size(aparams.B,1);

sigma = qparams.viscosity;

rv=x_it(1:nv); rp=x_it(nv+1:nv+np);

%% pressure solve
xB = (qparams.Gdiag\aparams.B')';
BCWeights = qparams.weights;
weight = min(diag(qparams.weights));

if qparams.domain==1,
   n_null = qparams.n_null;
   minor = [1:n_null-1,n_null+1:np]';
   rp1 = zeros(np,1);
   rp1(minor) = amg_v_cycle(rp(minor), amg_gridAw,  amg_smootherAw);
   rp2 =  xB*(aparams.F*(BCWeights*(xB'*rp1))) + weight*sigma*(qparams.Cp2*rp1);
   zp = zeros(np,1);
   zp(minor) =  - amg_v_cycle(rp2(minor), amg_gridA,  amg_smootherA);
else 
   rp1 = amg_v_cycle(rp, amg_gridAw,  amg_smootherAw);
   rp2 =  xB*(aparams.F*(BCWeights*(xB'*rp1))) + weight*sigma*(qparams.Cp2*rp1);
   zp =  - amg_v_cycle(rp2, amg_gridA,  amg_smootherA);
end


%% velocity solve
rv = rv-(aparams.B')*zp;
zv = amg_v_cycle(rv, amg_gridF,  amg_smootherF);
y = [zv;zp];
