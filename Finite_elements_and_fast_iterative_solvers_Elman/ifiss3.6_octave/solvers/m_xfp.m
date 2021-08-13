function y = m_xfp(x_it,aparams,mparams)
%M_XFP modified ideal PCD preconditioner
%   y = m_xfp(x_it,aparams,mparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%   output
%          y            result of preconditioning operation
%
%   IFISS function: DJS; 20 October 2009, revised HCE 27 September 2012,
%                                                      6 June 2013
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

nv = length(aparams.F);
np = size(aparams.B,1);

rv=x_it(1:nv); rp=x_it(nv+1:nv+np);

%% pressure solve
%% For enclosed flow:  
%   project intermediate term yp = Fp Mp \ rp into orthogonal complement
%   of null space of Ap, then apply action of Ap-inverse in a stable way
if mparams.domain==1,
   n_null = mparams.n_null;
   minor = [1:n_null-1,n_null+1:np]';
   yp = (mparams.Fp)*((mparams.Mp)\rp);
   yp = yp - sum(yp)/np * ones(np,1);
   zp = zeros(np,1);
   zp(minor) = - mparams.Ap(minor,minor) \ yp(minor);
else
   zp = -(mparams.Ap)\((mparams.Fp)*((mparams.Mp)\rp));
end

%% velocity solve
rv = rv-(aparams.B')*zp;
zv = aparams.F \ rv;
y = [zv;zp];