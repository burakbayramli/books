function results = lm_f_err_c(y,x,W1,W2,N)
% PURPOSE: Computes LM test for the presence of a SAR specification when
% autocorrelation is already accounted for under the form of a SEM
% specification. 
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
% slow moving index (all individuals for period 1, all individuals for
% period 2,...

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

% Transformation of the W matrices under the sparse format. 
W1s=sparse(W1);
W2s=sparse(W2);
[nt k]=size(x);
info.lflag=0;
res = sem_panel_FE_LY(y,x,W2,N,info);
y=res.ytrans;
x=res.xtrans;
% y is the transformed data vector of dimension N(T-1)(The transformation 
% of the data according to the Lee and Yu methodology has been performed in
% the sem_panel_FE_LY function). The same remark applies for x.

T=size(y,1)/N;
resid=res.resid;
rho=res.rho;
sige=res.sige;
beta= res.beta;
It=speye(T);
Bn=speye(N)-rho*W2s;
Bt=kron(It,Bn);

W1star=kron(It,W1s);
epB=resid'*Bt;
Wsytil=W1star*y;
numnum=epB*Wsytil;

num=(numnum/sige)^2;

%Computation of the denominator (Inverse of the VC matrix)
% Compute first the three elements of the I_11 matrix
% first element
W1sp=W1s';
tr1=T*sum(sum(W1s.*W1sp,2),1);

% Second element
xbet=x*beta;
Wxbet=W1star*xbet;
BWxbet=Bt*Wxbet;
tr2=1/sige*BWxbet'*BWxbet;

%Last term
Bi=inv(Bn);
A=Bn*W1s*Bi;
W3=A.*A; % We just take the square elements of W and sum them. 
tr3 = T*sum(sum(W3,2),1);
% I_11 matrix

I_11=tr1+tr2+tr3; 

% Computation of the I_12 matrix
 WBnpBn=W1s' *Bn'*Bn;
 BpB=kron(It,WBnpBn);
 BpBxb=BpB*x;
term1=(1/sige)*beta'*x'*BpBxb;
WBi=W2s*Bi;
term2=T*trace(WBi'*A)+T*trace(W2s*W1s*Bi);
term3=1/sige*T*trace(A);
% I_12 matrix

I_12=[term1 term2 term3];

% Computation of the variance-covariance matrix for the unrestricted set of
% parameter (beta, lambda (the rho in this program) and sigma2) 
I22=res.cov2;

%Computation of the test
den=I_11-I_12*I22*I_12';
lm=num/den;
prob=1-chis_prb(lm,1);


results.meth='lm_f_err_c';
results.lm=lm;
results.prob=prob;
results.Wsar= inputname(3);
results.Wsem=inputname(4);
results.nvar= k;
results.chi_1= 6.635;

