function y = m_bfbt(x_it,aparams,mparams)
%M_BFBT ideal LSC preconditioner (unscaled)
%   y = m_bfbt(x_it,aparams,mparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%   output
%          y            result of preconditioning operation
%
%   IFISS function: HCE; 15 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

nv = length(aparams.F);
nu = nv/2;
np = size(aparams.B,1);

rv=x_it(1:nv); rp=x_it(nv+1:nv+np);

%% pressure solve
BBt = (aparams.B)*(aparams.B');
   
if mparams.domain==1,
   n_null = mparams.n_null;
   minor = [1:n_null-1,n_null+1:np]';
   rp1 = zeros(np,1);
   rp1(minor) = BBt(minor,minor)\rp(minor);
   rp2 =  aparams.B*(aparams.F*(aparams.B'*rp1));
   zp = zeros(np,1);
   zp(minor) = - BBt(minor,minor)\rp2(minor);
else    
   zp = -BBt \ (aparams.B*(aparams.F*(aparams.B'*(BBt\rp))));
end 

%% velocity solve
rv = rv-(aparams.B')*zp;
zv = aparams.F \ rv;
y = [zv;zp];
