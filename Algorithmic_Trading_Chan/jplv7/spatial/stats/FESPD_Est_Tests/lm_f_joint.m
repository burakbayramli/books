function results=lm_f_joint(y,x,W,W2,N)
% PURPOSE : computation of a LM test to test for the presence of spatial
% autocorrelation. It's a joint test. 
% The input of the test must be the original data. The Lee and Yu
% transformation is applied inside the test.

% y : NT x 1 vector of dependent variable 
% x : NT x k matrix of independent variables (WITHOUT constant)
% W : (N x N) weight matrix for the autoregressive part 
% W2 : weight matrix for spatially autoregressive errors 
% Note :  the code has been written assuming possibly different Weight matrices for both the
% SAR and SEM parts.
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
[nt k] = size(x);
% Transformation of the data according to the Lee and Yu methodology

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

T=size(y,1)/N;  % It equals T-1 since the sample is shrunk from one period

% Estimation of the the pseudo within specification
fe=ols(y,x); 
resfe=fe.resid;
bw=fe.beta;
vew=resfe'*resfe/(N*T);% We use N*T since it is adjusted for the shrink in the sample size due to the Lee and Yu transformation (see Working paper 2008) 

It=eye(T);
Ws=sparse(W);
Wst=kron(It,Ws);
W2s=sparse(W2);
W2st=kron(It,W2s);

% Computation of Re
Re=resfe'*W2st*resfe/vew;

% Computation of Ry
Ry=resfe'*Wst*y/vew;

%Computation of the trace elements (T_22, T_12, T_11) 
%T_11

%first element (tr(WW))
Wsp=Ws';
tr1a=T*sum(sum(Ws.*Wsp,2),1);

%Second element (tr(W'W))
WW=Ws.*Ws; % We just take the square elements of W and sum them. 
tr1b = T*sum(sum(WW,2),1);
T_11=tr1a+tr1b;

%T_22
%firt element (tr(WW))
W2sp=W2s';
tr2a=T*sum(sum(W2s.*W2sp,2),1);

%Second element (tr(W'W))
W2W2=W2s.*W2s; % We just take the square elements of W and sum them. 
tr2b = T*sum(sum(W2W2,2),1);
T_22=tr2a+tr2b;

%T_12
%first element (tr(W1W2))
tr12a=T*sum(sum(Ws.*W2sp,2),1);

%Second element (tr(W'W2))
WW2=Ws.*W2s; % We just take the square elements of W and sum them. 
tr12b = T*sum(sum(WW2,2),1);
T_12=tr12a+tr12b;


M=speye(N*T)-x*inv(x'*x)*x';
xwbet=x*bw;
xwb=Wst*xwbet;

% Computation of the test
D=1/vew*xwb'*M*xwb;
E=(D+T_11)*T_22-(T_12*T_12);
lm=(1/E)*((T_22*(Ry*Ry))-(2*T_12*Re*Ry)+((Re*Re)*(D+T_11)));
prob = 1-chis_prb(lm,2);


results.meth='lm_f_joint';
results.lm=lm;
results.prob=prob;
results.Wsar= inputname(3);
results.Wsem=inputname(4);
results.nvar = k;
results.chi_1 = 9.210;





