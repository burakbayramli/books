function y = m_tfp(x_it,aparams,mparams)
%M_TFP modified ideal PCD preconditioner
%   y = m_tfp(x_it,aparams,mparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%   output
%          y            result of preconditioning operation
%
%   IFISS function: DJS; 20 October 2009.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

nv = length(aparams.F);
%nu = nv/2;
np = size(aparams.B,1);


rv=x_it(1:nv); rp=x_it(nv+1:nv+np);

%% pressure Poisson setup
Gdiag=spdiags(diag(mparams.G),0,nv,nv);
xB = (Gdiag\aparams.B')';
BBt = aparams.B*xB'; 

%% pressure
zp = -(BBt)\((mparams.Fp)*((mparams.Mp)\rp));

%% velocity solve
rv = rv-(aparams.B')*zp;
zv = aparams.F \ rv;
y = [zv;zp];
return
