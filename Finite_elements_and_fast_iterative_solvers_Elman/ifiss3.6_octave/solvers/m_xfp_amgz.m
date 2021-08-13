function y = m_xfp_amgz(x_it,aparams,mparams)
%M_XFP_AMGZ AMG iterated modified PCD preconditioner
%   y = m_xfp(x_it,aparams,mparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%   output
%          y            result of preconditioning operation
%
%   calls function amg_v_cycle 
%   Global array variables amg_gridA and amg_gridF define the
%   data for amg_v_cycle for Ap and F respectively
%   IFISS function: DJS; 1 November 2009, HCE 12 November 2012, 6 July 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage


global  amg_gridA amg_smootherA amg_gridF amg_smootherF
nv = length(aparams.F);
np = size(aparams.B,1);

rv=x_it(1:nv); rp=x_it(nv+1:nv+np);

%% AMG approximation to pressure solve zp = -Ap \ Fp * (Mp\rp)
%% For enclosed flow:  
%   project intermediate term rp1 = Fp Mp \ rp into orthogonal complement
%   of null space of Ap, then apply AMG to result
if mparams.domain==1,
   n_null = mparams.n_null;
   minor = [1:n_null-1,n_null+1:np]';
   rp1 = mparams.Fp*(diag(mparams.Mp).\rp);
   rp1 = rp1 - sum(rp1)/np * ones(np,1);
   zp = zeros(np,1);
   zp(minor) = amg_v_cycle(-rp1(minor), amg_gridA, amg_smootherA);
else 
   zp = amg_v_cycle(-mparams.Fp*(diag(mparams.Mp).\rp), amg_gridA, amg_smootherA);
end

%% AMG approximation to velocity solve
rv = rv-(aparams.B')*zp;
zv = amg_v_cycle(rv, amg_gridF,  amg_smootherF);
y = [zv;zp];