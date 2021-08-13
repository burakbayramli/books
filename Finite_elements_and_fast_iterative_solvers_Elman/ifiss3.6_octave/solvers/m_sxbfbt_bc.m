function y = m_sxbfbt_bc(x_it,aparams,mparams)
%M_SXBFBT_BC boundary adjusted ideal stabilized LSC 
%   y = m_xbfbt_bc(x_it,aparams,mparams);
%   input
%          x_it         operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%   output
%          y            result of preconditioning operation
%
%   IFISS function: HCE; 26 September 2012.
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage

nv = length(aparams.F);
%nu = nv/2;
np = size(aparams.B,1);
Gdiag=spdiags(diag(mparams.G),0,nv,nv);

sigma = mparams.viscosity;

rv=x_it(1:nv); rp=x_it(nv+1:nv+np);

%% pressure solve
xB = (Gdiag\aparams.B')';
BBt = aparams.B*xB';
BBt = BBt.*(abs(BBt)>5.d-15);
BCWeights = mparams.weights;
weightedBBt = aparams.B*(BCWeights*xB');
weightedBBt = weightedBBt.*(abs(weightedBBt)>5.d-15);
weight = min(diag(mparams.weights));

if mparams.domain==1,
   n_null = mparams.n_null;
   minor = [1:n_null-1,n_null+1:np]';
   rp1 = zeros(np,1);
   rp1(minor) = (weightedBBt(minor,minor)+weight*mparams.Cp1(minor,minor))\rp(minor);
   rp2 =  xB*(aparams.F*(BCWeights*(xB'*rp1))) + weight*sigma*(mparams.Cp2*rp1);
   zp = zeros(np,1);
   zp(minor) = - (BBt(minor,minor)+mparams.Cp1(minor,minor))\rp2(minor);
else
   zp = (weightedBBt+weight*mparams.Cp1)\rp;
   zp = -(BBt+mparams.Cp1) \ (xB*(aparams.F*(BCWeights*(xB'*zp))) + ...
           weight*sigma*(mparams.Cp2*zp) );
end 

%% velocity solve
rv = rv-(aparams.B')*zp;
zv = aparams.F \ rv;
y = [zv;zp];
