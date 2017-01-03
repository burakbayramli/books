function results = lm_f_sar_c(y,x,W1,W2,N)
% PURPOSE : LM test to check the presence of a residual error spatial
% autocorrelation when a SAR specification is already accounted for. 
% The input of the test are original data. The transformation is appplied 
% within the test.

% y : NT x 1 vector of dependent variable 
% x : NT x k matrix of independent variables (WITHOUT constant) 
% W1 : (N x N) weight matrix associated with the SAR specification
% W2 : (N x N) weight matrix associated with errors possibly spatially
%      autocorrelated
% Note that one can use the same matrix for the two specifications (W1=W2)
% N : Number of individuals

% Data are organised as follows : i is the fast moving index and t is the
% slow moving index (all individuals  for period 1, all individuals for
% period 2, ...
% If data are organized the other way (t fast moving and i slow moving),
% just apply the function trans_tslow to get the right organization

% Code has been programmed for balanced panel
%--------------------
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
% REFERENCE :
% Debarsy N, C. Ertur (2009), Testing for spatial autocorrelation in a fixed effects
% panel data model, Document de recherche du LEO, 2009-12. 

% -------------------------------------------------------------------- 

[nt k]=size(x);
T=nt/N;

%Do SAR fixed effect regressions to get the residuals
info.lflag=0;
res = sar_panel_FE_LY(y,x,W1,N,info);
e=res.resid; 
sige = res.sige; % Residual variance (no dof correction)
it = speye(T-1); % we use (T-1) because in the sar_panel_FE_LY function, 
 % we transformed data to fit the Lee and Yu methodology. Hence, the sample
 % shrinks from one period of time. NT->N(T-1)

W1s=sparse(W1);
W2s=sparse(W2);
iw2=kron(it,W2s);

rho=real(res.rho);

%Recovering the variance of rho. 
tmp= res.tstat(k+1,1);
tmps=tmp/rho;
tmps=1/tmps;
varp=tmps*tmps; %variance of rho.
A = speye(N)-rho*W1s;
AI=inv(A);

% Computation of the trace operator.
%first element (tr(WW))
W2sp=W2s';
tr1=(T-1)*sum(sum(W2s.*W2sp,2),1);

%Second element (tr(W'W))
W2=W2s.*W2s;
tr2 = (T-1)*sum(sum(W2,2),1);
T_22=tr1+tr2;

Tlr1=(T-1)*trace(W2'*W1*AI);
Tlr2=(T-1)*trace(W2*W1*AI);
Tlr=Tlr1+Tlr2;

% Computation of the test.
lm1=e'*iw2*e/sige;
num = lm1*lm1;
den = T_22-(Tlr)*(Tlr)*varp;
lm=num/den;
prob = 1-chis_prb(lm,1);

results.meth='lm_f_sar_c';
results.lm=lm;
results.prob = prob;
results.Wsar= inputname(3);
results.Wsem=inputname(4);
results.nvar = k;
results.chi_1= 6.635;
