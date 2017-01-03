function results=lm_f_err(y,x,W,N)
% PURPOSE : computation of the LM test in a fixed effect panel data
% framework to see whether spatial autocorrelation is present under the form
% of a SAR. 
% The input of the tests are original data. The transformation is
% applied within the code.

% y : NT x 1 vector of dependent variable 
% x : NT x k matrix of independent variables (WITHOUT constant)
% W : (N x N) weight matrix
% N : number of individuals.

% Data organisation : i is fast moving index and t is slow moving index (all 
% individuals  for period 1, all individuals for period 2, ...). If it is not 
% the case,apply the function trans_tslow.m 

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

% Transformation of the data according to the Lee and Yu methodology



[nt k] = size(x);
T=nt/N; 
IT=eye(T);
IN=eye(N);
Jt=ones(T);
Jbar=1/T*Jt;
Q=IT-Jbar;
[V,D]=eig(Q);
D=diag(D);
j=find(D<=0.0001&D>=-0.0001);
V(:,j)=[];
F=V;
TR=kron(F',IN);
% Applying the Lee and Yu transformation
y=TR*y;
x=TR*x;

T=size(y,1)/N;  %It equals T-1 since the sample is shrunk from one period

% Estimation of the the pseudo within specification
fe = ols(y,x);

resfe=fe.resid;
vew=resfe'*resfe/(N*T);

Ws=sparse(W);
it=speye(T);

Wk=kron(it,Ws);
lme=resfe'*Wk*resfe/vew;

%first element (tr(WW))
Wsp=Ws';
tr1=T*sum(sum(Ws.*Wsp,2),1);

%Second element (tr(W'W))
W2=Ws.*Ws;  
tr2 = T*sum(sum(W2,2),1);

T_22=tr1+tr2;

% Computation of the test.
num = lme*lme;
den = T_22;
lm=num/den;
prob=1-chis_prb(lm,1);

results.meth='lm_f_err';
results.lm=lm;
results.prob=prob;
results.Wsem= inputname(3);
results.nvar = k; 
results.chi_1= 6.635;