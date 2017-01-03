function results =lr_f_joint(y,x,W,W2,N)
% PURPOSE : Computes the LR statistic to test the presence of spatial
% autocorrelation in a fixed effects panel data model. 
% Under the null, both lambda and rho are null
% while under the alternative, at least one is different from zero. For this
% test, we chose as unconstrained model the general one (allowing both rho and lambda to 
% be different from zero) (SAC model) 

% y : (NT x 1) vector of the dependent variable
% x : (NT x K) matrix of independent variables (WITHOUT CONSTANT)
% W : (N x N) weight matrix for the autoregressive part  
% W2 : (N x N) weight matrix for spatially autoregressive errors 
% Note :  the code has been written assuming possibly different Weight matrices for both the
% SAR and SEM parts.
% N : number of individuals.

% Data organisation : i is fast moving index and t is slow moving index (all 
% individuals  for period 1, all individuals for period 2, ...). 
% If it is not the case,apply the function trans_tslow.m 

% The input of the test must be the original data. The Lee and Yu
% transformation is applied inside the test.

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
%---------------------------------------------------------



% Computation of the unconstrained model (SAC estimation)
info.lflag=0;
res1 = sarar_panel_FE_LY(y,x,W,W2,N,info);
liku=res1.lik;

% Constrained model (estimated by OLS but without DoF correction since the
% model should be estimated by ML)

% Transformation of the data according to the Lee and Yu methodology
[NT k]=size(x);
T=NT/N;
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
NT1=size(y,1);
% Computation of the constrained model
res2=ols(y,x);
resid=res2.resid;

% Computation of the log likelihood value. 

likr=-(NT1/2)*log(2*pi) - (NT1/2)*log((resid'*resid)/NT1) -NT1/(2*resid'*resid)*resid'*resid; 
% Note that we take the log(2*pi). It is because other panel functions use
% it. It's important to remember if one has to compare likelihood
% values. 

% Computation of the test
lr=2*(liku-likr);
prob=1-chis_prb(lr,2);

results.meth='lr_f_joint';
results.lr=lr;
results.prob=prob;
results.Wsar=inputname(3);
results.Wsem=inputname(4);
results.nvar = k;
results.chi_1 = 9.210;


