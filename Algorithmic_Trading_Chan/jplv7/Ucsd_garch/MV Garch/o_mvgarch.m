function [parameters, Ht,  stdresid, stderrors, A, B, weights, principalcomponets, cumR2]=o_mvgarch(data, numfactors,archP,garchQ);
% PURPOSE:
%        Estimates a multivariate GARCH model using Orthogonal or Factor Garch.  Involves PCS to 
%        reduce volatility modelling to univariate garches.  See Carrol 2000(An Inrtoduction to O-Garch)
% 
% USAGE:
%        [parameters, Ht,  stdresid, stderrors, A, B, weights, principalcomponets, cumR2]=o_mvgarch(data, numfactors,archP,garchQ);
% 
% 
% INPUTS:
%      data:   A zero mean t by k vector of residuals from some filtration
%      numfactors : The number of Principal COmponets to include in the MV_GARCH model
%                   *****  NOTE: The conditional covariances will be singluar unless numfactors == k
%      archP:  One of three things:    Empty   in which case a 1 innovation model is estimated for each factor
%                                      A scalar, p     in which case a p innovation model is estimated for each factor
%                                      A k by 1 vector in which case the ith series has innovation terms p=archP(i)
%      garchQ:  One of three things:    Empty   in which case a 1 GARCH lag is used in estimation for each factor
%                                      A scalar, q     in which case a q GARCH lags is used in estimation for factor
%                                      A k by 1 vector in which case the ith series has lagged variance terms q=archQ(i)
% 
% OUTPUTS:
%      parameters= A vector of parameters estimated form the model of the form
%                  [GarchParams(1) GarchParams(2) ... GarchParams(k)]
%                  where the garch parameters from each estimation are of the form
%                  [omega(i) alpha(i1) alpha(i2) ... alpha(ip(i)) beta(i1) beta(i2) ... beta(iq(i))]
%      Ht= A k by k by t array of conditional variances
%      stdresid = The multivariate standardized residuals(only if numfactors==k)
%      stderrors=A length(parameters)^2 matrix of estimated robust standard errors
%      A = The estimated A in the robust standard errors(Hessian of LLF)
%      B = The estimated B from the standard errors
%      scores = The estimated scores of the factor GARCH likelihoods t by length(parameters)
%      weights            - a k by k matrix of componet weights
%      principalcomponets - a t by k matrix of principal componets
%      cumR2              - The cumulative R2 of including the ith PC
% 
% COMMENTS:
%     Uses PCA from the ucsd_garch toolbox
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


[t,k]=size(data);

if isempty(archP)
    archP=ones(1,k);
elseif length(archP)==1
    archP=ones(1,k)*archP;
end

if isempty(garchQ)
    garchQ=ones(1,k);
elseif length(garchQ)==1
    garchQ=ones(1,k)*garchQ;
end


[weights, principalcomponets, eigenvalues,explainedvariance, cumR2]=pca(data,'Corr');

H=zeros(size(data));
options=optimset('fmincon');
options=optimset(options,'Display','off','Diagnostics','off','MaxFunEvals',1000*max(archP+garchQ+1),'MaxIter',1000*max(archP+garchQ+1),'LargeScale','off');

for i=1:numfactors
    fprintf(1,'Estimating GARCH model for Factor %d\n',i)
    [univariate{i}.parameters, univariate{i}.likelihood, univariate{i}.stderrors, univariate{i}.robustSE, univariate{i}.ht, univariate{i}.scores] ... 
        = fattailed_garch(principalcomponets(:,i) , archP(i) , garchQ(i) , 'NORMAL',[], options);
    H(:,i)=univariate{i}.ht;
end
StdDev=std(data);
normweights=weights.*repmat(StdDev',1,k);
Ht=zeros(k,k,t);
for i=1:t
    Ht(:,:,i)=normweights(:,1:numfactors)*diag(H(i,1:numfactors))*normweights(:,1:numfactors)';
end


parameters=[];
for i=1:numfactors;
    parameters=[parameters;univariate{i}.parameters];
end

A=zeros(length(parameters),length(parameters));
jointscores=zeros(t,length(parameters));
index=1;
for i=1:numfactors
    workingsize=size(univariate{i}.stderrors);
    A(index:index+workingsize-1,index:index+workingsize-1)=univariate{i}.stderrors^(-1);
    jointscores(:,index:index+workingsize-1)=univariate{i}.scores;
    index=index+workingsize;
end
B=cov(jointscores);
scores=jointscores;
stderrors=A^(-1)*B*A^(-1)*t;
A=(A/t);

if numfactors==k
    stdresid=zeros(t,k);
    for i=1:t
        stdresid(i,:)=data(i,:)*Ht(:,:,i)^(-0.5);
    end
else
    stdresid=[]
end