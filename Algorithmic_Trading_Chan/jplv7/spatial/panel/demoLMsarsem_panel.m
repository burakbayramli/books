clear all;

A=wk1read('cigardemo.wk1',1,0);
W1=wk1read('Spat-Sym-US.wk1');
% Dataset downloaded from www.wiley.co.uk/baltagi/
% Spatial weights matrix constructed by Elhorst
%
% written by: J.Paul Elhorst summer 2008
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCES: 
% Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.
%
% dimensions of the problem
T=6; % number of time periods
N=46; % number of regions
% row-normalize W
W=normw(W1); % function of LeSage
y=A(:,[3]); % column number in the data matrix that corresponds to the dependent variable
x=A(:,[4,5,6]); % column numbers in the data matrix that correspond to the independent variables
xconstant=ones(N*T,1);
[nobs K]=size(x);
% ----------------------------------------------------------------------------------------
% ols estimation 
results=ols(y,[xconstant x]);
vnames=strvcat('logcit','intercept','logp','logpn','logy');
prt_reg(results,vnames,1);
sige=results.sige*((nobs-K)/nobs);
loglikols=-nobs/2*log(2*pi*sige)-1/(2*sige)*results.resid'*results.resid
LMsarsem_panel(results,W,y,[xconstant x]); % (Robust) LM tests
% ----------------------------------------------------------------------------------------
% spatial fixed effects + (robust) LM tests for spatial lag and spatial error model
% fixed effects, within estimator
% demeaning of the y and x variables
model=1;
[ywith,xwith,meanny,meannx,meanty,meantx]=demean(y,x,N,T,model);
results=ols(ywith,xwith);
vnames=strvcat('logcit','logp','logpn','logy'); % should be changed if x is changed
prt_reg(results,vnames);
FE=meanny-meannx*results.beta; % including the constant term
yme = y - mean(y);
ee=ones(T,1);
error=y-kron(ee,FE)-x*results.beta;
rsqr1 = error'*error;
rsqr2 = yme'*yme;
FE_rsqr2 = 1.0 - rsqr1/rsqr2 % r-squared including fixed effects
sige=results.sige*((nobs-K)/nobs);
loglikfe=-nobs/2*log(2*pi*sige)-1/(2*sige)*results.resid'*results.resid
LMsarsem_panel(results,W,ywith,xwith); % (Robust) LM tests
% ----------------------------------------------------------------------------------------
% spatial and time period fixed effects + (robust) LM tests for spatial lag and spatial error model
% fixed effects, within estimator
% demeaning of the y and x variables
model=3;
[ywith,xwith,meanny,meannx,meanty,meantx]=demean(y,x,N,T,model);
results=ols(ywith,xwith);
vnames=strvcat('logcit','logp','logpn','logy'); % should be changed if x is changed
prt_reg(results,vnames);
LMsarsem_panel(results,W,ywith,xwith); % (Robust) LM tests
