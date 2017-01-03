function  llike=f_sarar_panel(parm,y,x,W1,W2,det1,det2,T)
%PURPOSE : evaluates the log-likelihood for general spatial fixed effect 
% panel model
% parm = (rho;lam)
% y : dependent variable
% x :matrix of explanatory variables
% W1 : Spatial weight matrix associated with autoregressive part (Wy)
% W2 : Spatial weight matrix associated with the SAR error component
% det1 : matrix with [rho log determinant] values computed in sac_panel_FE_LY.m 
%        using one of the Pace and Barry's routine
% det2 : matrix with [lam log determinant] values computed in sac_panel_FE_LY.m 
%        using one of the Pace and Barry's routine

% RETURNS: a scalar equal to minus the log-likelihood  function value at
% the parameters rho,lambda

%------------------------------------
% Written by N. Debarsy* and C. Ertur**
% * University of Namur
%   Centre de recherches en Economie Régionale et Politique Economique (CERPE)
%   Rempart de la vierge, 8
%   5000 Namur, Belgium
%   nicolas.debarsy@fundp.ac.be

%** Université d'Orléans
%   UFR Droit-Economie-Gestion
%   Laboratoire d'Economie d'Orléans - UMR 6221 CNRS
%   Domaine Universitaire
%   Rue de Blois - BP 6739
%   45067 ORLEANS Cedex 2, France
%   cem.ertur@univ-orleans.fr
% This function is based on James P. LeSage's function f2_sac.m 

%-------------------------------------------------------------
rho=parm(1,1);
lam=parm(2,1);
NT1=length(y);
N=NT1/T;
gsize = det1(2,1) - det1(1,1);
 i1 = find(det1(:,1) <= rho + gsize);
 i2 = find(det1(:,1) <= rho - gsize);
 i1 = max(i1);
 i2 = max(i2);
 index = round((i1+i2)/2);
if isempty(index)
index = 1;
end;
 detval1 = det1(index,2);

 gsize = det2(2,1) - det2(1,1);
 i1 = find(det2(:,1) <= lam + gsize);
 i2 = find(det2(:,1) <= lam - gsize);
 i1 = max(i1);
 i2 = max(i2);
 index = round((i1+i2)/2);
if isempty(index);
index = 1;
end;
 detval2 = det2(index,2);
 
In = speye(N);
It= speye(T);
Ay = kron(It,(In - rho*W1))*y;
B=kron(It,(In-lam*W2));
b = (B*x)\(B*Ay);

e = B*(Ay - x*b);
epe = e'*e; 
llike = (NT1/2)*log(epe/NT1) - T*detval1 - T*detval2;
