function results=lm_f_sar(y,x,W,N)
% PURPOSE : computation of the LM test in a fixed effect panel data
% framework to see whether spatial autocorrelation is present under the form
% of a SAR.
% The input of the test must be the original data. The Lee and Yu
% transformation is applied inside the test.

% y : NT x 1 vector of dependent variable 
% x : NT x k matrix of independent variables (WITHOUT constant)
% W : (N x N) weight matrix
% N : number of individuals.

% Data organisation : i is fast moving index and t is slow moving index (all 
% individuals  for period 1, all individuals for period 2, ...). 
% If it is not the case,apply the function trans_tslow.m 

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
% ((Journal of Econometrics, 2009)


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

T=size(y,1)/N; % It equals T-1 since the sample is shrunk from one period

% Estimation of the the pseudo within specification
fe=ols(y,x); 
resfe=fe.resid;
b=fe.beta; %estimated coefficient
vew=(resfe'*resfe)/(N*T); %residual variance of the transformed data 


int=speye(N*T);
M = int-x*inv(x'*x)*x';
it=eye(T);
Ws=sparse(W);
Wk=kron(it,Ws);

% Computation of the trace term
Wsp=Ws';
tr1=T*sum(sum(Ws.*Wsp,2),1);

%Second element (tr(W'W))
W2=Ws.*Ws; 
tr2 = T*sum(sum(W2,2),1);

T_11=tr1+tr2;

% Computation of the test.
D=(1/vew)*(Wk*x*b)'*M*(Wk*x*b);
lml=resfe'*Wk*y/vew;
num = lml*lml;
den = D+T_11;
lm = num/den;
prob = 1-chis_prb(lm,1);

results.meth='lm_f_sar';
results.lm=lm;
results.prob=prob;
results.Wsar= inputname(3);
results.nvar = k;;
results.chi_1= 6.635;