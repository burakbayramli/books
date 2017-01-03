function [parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores]  = full_bekk_mvgarch(data,p,q, BEKKoptions)
% PURPOSE:
%      To Estimate a full BEKK multivariate GARCH model.  ****SEE WARNING AT END OF HELP FILE****
% 
% 
% USAGE:
%      [parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores]  = full_bekk_mvgarch(data,p,q,options);
% 
% 
% INPUTS:
%      data          - A t by k matrix of zero mean residuals
%      p             - The lag length of the innovation process
%      q             - The lag length of the AR process
%      options       - (optional) Options for the optimization(fminunc)
% 
% OUTPUTS:
%      parameters    - A (k*(k+1))/2+p*k^2+q*k^2 vector of estimated parameteters. F
%                         or any k^2 set of Innovation or AR parameters X, 
%                         reshape(X,k,k) will give the correct matrix
%                         To recover C, use ivech(parmaeters(1:(k*(k+1))/2)
%      loglikelihood - The loglikelihood of the function at the optimum
%      Ht            - A k x k x t 3 dimension matrix of conditional covariances
%      likelihoods   - A t by 1 vector of individual likelihoods
%      stdresid      - A t by k matrix of multivariate standardized residuals
%      stderrors     - A numParams^2 square matrix of robust Standad Errors(A^(-1)*B*A^(-1)*t^(-1))
%      A             - The estimated inverse of the non-robust Standard errors
%      B             - The estimated covariance of teh scores
%      scores        - A t by numParams matrix of individual scores
% 
% 
% COMMENTS:
%    You should multiply the data by a constant so that the min std(data) is at least 10.  This will help estimation
%
%      ***************************************************************************************
%      *  THIS FUNCTION INVOLVES ESTIMATING MANY PARAMETERS.  THE EXACT NUMBER OF PARAMETERS 
%      *  NEEDING TO BE ESTIMATED IS (k*(k+1))/2+pk^2+qk^2.  FOR A 5 VARIATE (1,1) MODEL THIS 
%      *  65 PARAMETERS.  ESTIMATION CAN TAKE A VERY LONG TIME.  A 10 ASSET MODEL TOOK 12 
%      *  HOURS ON A PIII-700.
%      ***************************************************************************************
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
 



% need to try and get some smart startgin values
if size(data,2) > size(data,1)
    data=data';
end

[t k]=size(data);
k2=k*(k+1)/2;

scalaropt=optimset('fminunc');
scalaropt=optimset(scalaropt,'TolFun',1e-1,'Display','iter','Diagnostics','on','DiffMaxChange',1e-2);
startingparameters=scalar_bekk_mvgarch(data,p,q,scalaropt);
CChol=startingparameters(1:(k*(k+1))/2);
%C=ivech(startingparameters(1:(k*(k+1))/2))*ivech(startingparameters(1:(k*(k+1))/2))';
newA=[];
newB=[];
for i=1:p
    newA=[newA diag(ones(k,1))*startingparameters(((k*(k+1))/2)+i)]; %#ok<AGROW>
end
for i=1:q
    newB=[newB diag(ones(k,1))*startingparameters(((k*(k+1))/2)+i+p)]; %#ok<AGROW>
end
newA=reshape(newA,k*k*p,1);
newB=reshape(newB,k*k*q,1);
startingparameters=[CChol;newA;newB];

if nargin<=3 || isempty(BEKKoptions)
    options=optimset('fminunc');
    options.Display='iter';
    options.Diagnostics='on';
    options.TolX=1e-4;
    options.TolFun=1e-4;
    options.MaxFunEvals=5000*length(startingparameters);
    options.MaxIter=5000*length(startingparameters);   
else
    options=BEKKoptions;
end
parameters=fminunc('full_bekk_mvgarch_likelihood',startingparameters,options,data,p,q,k,k2,t);


[loglikelihood,likelihoods,Ht]=full_bekk_mvgarch_likelihood(parameters,data,p,q,k,k2,t);
loglikelihood=-loglikelihood;
likelihoods=-likelihoods;

% Standardized residuals
stdresid=zeros(size(data));
for i=1:t
    stdresid(i,:)=data(i,:)*Ht(:,:,i)^(-0.5);
end


%Std Errors
if nargout>=6
    A=hessian_2sided('full_bekk_mvgarch_likelihood',parameters,data,p,q,k,k2,t);
    
    h=max(abs(parameters/2),1e-2)*eps^(1/3);
    hplus=parameters+h;
    hminus=parameters-h;
    likelihoodsplus=zeros(t,length(parameters));
    likelihoodsminus=zeros(t,length(parameters));
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hplus(i);
        [HOLDER, indivlike] = full_bekk_mvgarch_likelihood(hparameters,data,p,q,k,k2,t);
        likelihoodsplus(:,i)=indivlike;
    end
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hminus(i);
        [HOLDER, indivlike] = full_bekk_mvgarch_likelihood(hparameters,data,p,q,k,k2,t);
        likelihoodsminus(:,i)=indivlike;
    end
    scores=(likelihoodsplus-likelihoodsminus)./(2*repmat(h',t,1));
    B=cov(scores);
    A=A/t;
    stderrors=A^(-1)*B*A^(-1)*t^(-1);
end
