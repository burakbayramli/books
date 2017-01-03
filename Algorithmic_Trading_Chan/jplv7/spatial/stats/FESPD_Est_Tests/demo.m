% PURPOSE: An example of using the fixed effects spatial panel estimation
% and spatial autocorrelation tests toolbox                        
%---------------------------------------------------
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

clear all;
% Creation of data
N=81;
T=8;
x=randn(N*T,3);
b=[1;2;3];
In=eye(N);
It=eye(T);
v=randn(N*T,1);
rho=0.7;
lam=-0.5;

% Loading the spatial weight matrix
load mat81q;
W=mat81q;
clear mat81q;

A=In-rho*W;
B=In-lam*W;
Ai=inv(A);
Bi=inv(B);

% Creation of the individual fixed effects
mu=-50+100*rand(N,1);
mu=kron(ones(T,1),mu);

%Generation of the DGP
y=kron(It,Ai)*(x*b+mu)+kron(It,Ai*Bi)*v;

%Use of the tests

%Joint LM test
res=lm_f_joint(y,x,W,W,N);
prt(res)

% LM test for spatially autocorrelated errors
res1=lm_f_err(y,x,W,N);
prt(res1)

% LM test for a SAR specification
res2=lm_f_sar(y,x,W,N);
prt(res2)

% LM test for SAR specification when spatially autocorrelated errors are
% already accounted for
res3=lm_f_err_c(y,x,W,W,N);
prt(res3)

% LM test for spatially autocorrelated errors when a SAR specification is
% already accounted for
res4=lm_f_sar_c(y,x,W,W,N);
prt(res4)

%Joint LR test
res5=lr_f_joint(y,x,W,W,N);
prt(res5)

% LR test for spatially autocorrelated errors
res6=lr_f_err(y,x,W,N);
prt(res6)

% LR test for a SAR specification
res7=lr_f_sar(y,x,W,N);
prt(res7)

% LR test for SAR specification when spatially autocorrelated errors are
% already accounted for
res8=lr_f_err_c(y,x,W,W,N);
prt(res8)

% LR test for spatially autocorrelated errors when a SAR specification is
% already accounted for
res9=lr_f_sar_c(y,x,W,W,N);
prt(res9)

% Estimation of fixed effects spatial panel data models
% Variables names
vnames=strvcat('dependent','x1','x2','x3');
info.lflag=0;

% Fixed effects SEM panel data estimation
res10=sem_panel_FE_LY(y,x,W,N,info);
prt(res10,vnames);

% Fixed effects SAR panel data estimation
res11=sar_panel_FE_LY(y,x,W,N,info);
prt(res11,vnames);

% Fixed effects SARAR panel data estimation
res12=sarar_panel_FE_LY(y,x,W,W,N,info);
prt(res12,vnames);
