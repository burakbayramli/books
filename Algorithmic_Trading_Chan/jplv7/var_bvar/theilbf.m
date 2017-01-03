function result = theilbf(xpy,xpx,nlag,neqs,eqn,theta,weight,decay,scale2,scale,nx)
% PURPOSE: do Theil-Goldberger for bvar model (called by bvarf.m, becmf.m)
%--------------------------------------------------------------                     
% USAGE:  result = theilbf(y,x,neqs,eqn,theta,weight,decay,scale2,scale,nx)                                 
% where:  y = nobs x 1 input vector                          
%         x = nobs x nvar input explanatory variables matrix 
%         nobs = # of observations                           
%         neqs = # of equations                              
%         eqn  = # equation number                           
%         theta= overall tightness                           
%         weight = scalar or (neqs x neqs) matrix of prior weights                        
%         decay  = lag decay                                 
%         scale  = scaling vector (determined in bvarf)                           
%         scale2 = scaling vector (determined in bvarf)  
%         nx     = # of deterministic variables excluding constant term
%--------------------------------------------------------------                                                
% RESULTS: a structure
%        result.beta  = bhat 
%--------------------------------------------------------------                                                
% NOTE:  This routine is the same as theilbv used by bvar except
%        it saves time by not computing ancillary statistics
%        because we only need bhat's for forecasting               
%--------------------------------------------------------------                     
% see also: bvarf(), varf()
%--------------------------------------------------------------                     

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
                          
[junk nvar] = size(xpx);

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
xpxrpr = xpx + R'*R;

% find xpxi
xpxi = inv(xpxrpr);

% form xpy + rpc
xpyrpc = xpy + R'*c;

% find bhat
result.beta = xpxi*xpyrpc;
    
