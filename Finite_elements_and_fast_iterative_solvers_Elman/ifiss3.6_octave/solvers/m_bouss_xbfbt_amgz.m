function y = m_bouss_xbfbt_amgz(x_it,aparams,qparams)
%M_BOUSS_XBFBT_AMGZ AMG iterated LSC preconditioner
%   y = m_bouss_xbfbt_amgz(x_it,aparams,qparams)
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix (F, B)
%          qparams      structure defining preconditioning operator
%                       (domain and Gdiag=diagonal of velocity mass matrix)
%   output
%          y            result of preconditioning operation
%
%   calls function amg_v_cycle 
%   Global array variables amg_gridA. amg_gridFt and amg_gridF define the
%   data for amg_v_cycle for Ap. Ft and F respectively
%   IFISS function:  27 April 2012.
% Copyright (c) 2012 D.J. Silvester, M.L. Mihajlovic.

global  amg_gridA amg_smootherA amg_gridF amg_smootherF amg_gridFt  amg_smootherFt
nv = length(aparams.F);
np = size(aparams.B,1); 
nt = size(aparams.Mt,2);
rv=x_it(1:nv); rp=x_it(nv+1:nv+np); rt=x_it(nv+np+1:nv+np+nt);

%% temperature zt = aparams.Ft \ rt;
zt = amg_v_cycle(rt, amg_gridFt,  amg_smootherFt);

%% pressure solve
xB = (qparams.Gdiag\aparams.B')';

qparams.domain=2; %%treat matrix as non-singular
if qparams.domain==1,
   n_null = qparams.n_null;
   minor = [1:n_null-1,n_null+1:np]';
   rp1 = zeros(np,1);
   rp1(minor) = amg_v_cycle(rp(minor), amg_gridA,  amg_smootherA);
   rp2 =  xB*(aparams.F*(xB'*rp1));
   zp = zeros(np,1);
   zp(minor) =  - amg_v_cycle(rp2(minor), amg_gridA,  amg_smootherA);
else 
   rp1 = amg_v_cycle(rp, amg_gridA,  amg_smootherA);
   rp2 =  xB*(aparams.F*(xB'*rp1));
   zp =  - amg_v_cycle(rp2, amg_gridA,  amg_smootherA);
end
qparams.domain=1;

%% velocity solve
rv = rv + (aparams.Mt)*zt -(aparams.B')*zp;
zv = amg_v_cycle(rv, amg_gridF,  amg_smootherF);
y = [zv;zp;zt];
return
