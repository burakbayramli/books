function result = theilbv(y,x,nlag,neqs,eqn,theta,weight,decay,scale2,scale,nx)
% PURPOSE: do Theil-Goldberger for bvar model (called by bvar.m)
%--------------------------------------------------------------                     
% USAGE:  result = theilbv(y,x,neqs,eqn,theta,weight,decay,scale2,scale,nx)                                 
% where:  y = nobs x 1 input vector                          
%         x = nobs x nvar input explanatory variables matrix 
%         nobs = # of observations                           
%         neqs = # of equations                              
%         eqn  = # equation number                           
%         theta= overall tightness                           
%         weight = scalar or (neqs x neqs) matrix of prior weights                        
%         decay  = lag decay                                 
%         scale  = scaling vector (determined in bvar)                           
%         scale2 = scaling vector (determined in bvar)  
%         nx     = # of deterministic variables excluding constant term
%--------------------------------------------------------------                                                
% RESULTS: a structure
%        result.beta  = bhat                 
%        result.tstat = t-statistics              
%        result.tprob = t-probabilities              
%        result.yhat  = yhat                 
%        result.resid = residuals            
%        result.sige  = e'*e/(n-k)                  
%        result.rsqr  = rsquared                    
%        result.rbar  = rbar-squared                
%        result.nobs  = nobs                        
%        result.nvar  = nvar   
%--------------------------------------------------------------                     
% see also: bvar(), prt_var(), plt_var()
%--------------------------------------------------------------                     

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
                          
[nobs nvar] = size(x);

result.nobs = nobs;
result.nvar = nvar;


[nw1 nw2] = size(weight);
if nw1 == 1  % case of a scalar symmetric weight matrix
wght = ones(neqs,neqs)*weight;
 for i=1:neqs;
 wght(i,i) = 1.0;
 end;
else % general prior weight matrix
wght = weight;
end;

% find Doan's sigma(i,j,l)
sigma = zeros(nvar,1);

k = 1;

for j=1:neqs;
 for l=0:nlag-1;
  ldecay = (l+1)^decay;
  ldecay = 1.0/ldecay;
  sigma(k,1) = (theta*wght(eqn,j)*ldecay)*scale2(j,eqn);
  k = k+1;
 end;
end;

% setup prior R-matrix
% R = diagonal matrix with scale(i,1)/S(i,j,l)
R = zeros(nvar,nvar);

% N.B. we don't want to divide by zero 
% (diffuse prior on the x-variables and constant term) 
% so we use nvar-nx-1  

for i=1:nvar-nx-1;
R(i,i) = scale(eqn,1)/sigma(i);
end;

% setup prior c-vector
% equal to scale(i,1)/S(i,j,l) x prior mean

c = zeros(nvar,1);
cind = (eqn-1)*nlag+1;
if eqn == 1
cind = 1;
end;
c(cind,1) = scale(eqn,1)/sigma(cind,1);

% form X'X + R'R
xpxrpr = x'*x + R'*R;

% find xpxi
xpxi = inv(xpxrpr);

% form xpy + rpc
xpyrpc = x'*y + R'*c;

% find bhat
result.beta = xpxi*xpyrpc;
    
% find y-hat
result.yhat = x*result.beta;

% find residuals
result.resid = y - result.yhat;

% find sige
sigu = result.resid'*result.resid;
result.sige = sigu/(nobs-1); % Litterman-Doan Bayesian dof

% find t-values and probabilities
tstat = result.sige*diag(xpxi);
result.tstat = result.beta./sqrt(tstat);
result.tprob = tdis_prb(result.tstat,nobs-1);

% find r-squared, rbar-squared
meany = mean(y);
ym = y - meany*ones(nobs,1);
rsqr1 = sigu;
rsqr2 = ym'*ym;
result.rsqr = 1 - (rsqr1/rsqr2);
rsqr1 = rsqr1/(nobs-nvar); % Doan uses nvar=1
rsqr2 = rsqr2/(nobs-1);
result.rbar = 1 - (rsqr1/rsqr2);


