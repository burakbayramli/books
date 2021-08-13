function y = m_fp_mg(x_it,aparams,mparams)
%M_FP_MG GMG iterated PCD preconditioner
%   y = m_fp_mg(x_it,aparams,mparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure containing all preconditioning data
%                       including Fp, Ap, Mp (mass matrix) and mgdata
%   output
%          y            result of preconditioning operation
%
%   IFISS function: HCE; 17 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

nv = length(aparams.F);
nu = nv/2;
np = size(aparams.B,1);

rv=x_it(1:nv); rp=x_it(nv+1:nv+np);

domain = mparams.domain;

%% pressure solve
mg_mat = 1;
%zp = -(mparams.Mp)\((mparams.Fp)*((mparams.Ap)\rp));
zp = mg_ns(mparams.Ap,rp, domain,mg_mat,mparams.mgdata_diff);
zp = -diag(diag(mparams.Mp))\((mparams.Fp)*zp);

%% velocity solve
mg_mat = 2;
rv = rv-(aparams.B')*zp;
%zvx = (aparams.F(1:nu,1:nu))\rv(1:nu);  
zvx = mg_ns(aparams.F(1:nu,1:nu),rv(1:nu), domain,mg_mat,mparams.mgdata_cd);   
%zvy = (aparams.F(1:nu,1:nu))\rv(nu+1:nv);
zvy = mg_ns(aparams.F(1:nu,1:nu),rv(nu+1:nv), domain,mg_mat,mparams.mgdata_cd); 

y = [zvx;zvy;zp]; 
