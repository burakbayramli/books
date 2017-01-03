function [parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores] = scalar_bekk_mvgarch(data,p,q,BEKKoptions)
% PURPOSE:
%      To Estimate a scalar BEKK multivariate GARCH model.  ****SEE WARNING AT END OF HELP FILE****
% 
% 
% USAGE:
%      [parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores]  = scalar_bekk_mvgarch(data,p,q,options);
% 
% 
% INPUTS:
%      data          - A t by k matrix of zero mean residuals
%      p             - The lag length of the innovation process
%      q             - The lag length of the AR process
%      options       - (optional) Options for the optimization(fminunc)
% 
% 
% OUTPUTS:
%      parameters    - A (k*(k+1))/2+p+q vector of estimated parameteters. 
%                         To recover C, use ivech(parmaeters(1:(k*(k+1))/2)
%      loglikelihood - The loglikelihood of the function at the optimum
%      Ht            - A k x k x t 3 dimension matrix of conditional covariances
%      likelihoods   - A t by 1 vector of individual likelihoods
%      stdresid      - A t by k matrix of multivariate standardized residuals
%      stderrors     - A numParams^2 square matrix of robust Standad Errors(A^(-1)*B*A^(-1)*t^(-1))
%      A             - The estimated inverse of the non-robust Standard errors
%      B             - The estimated covariance of the scores
%      scores        - A t by numParams matrix of individual scores
% 
% 
% COMMENTS:
%      ***************************************************************************************
%      *  THIS FUNCTION INVOLVES ESTIMATING QUITE A FEW PARAMETERS WHEN K IS LARGE.  THE EXACT 
%      *  NUMBER OF  PARAMETERS NEEDING TO BE ESTIMATED IS (k*(k+1))/2+p+q.  FOR A 10 VARIATE 
%      *  (1,1) MODEL THIS INVLOVES 56 PARAMETERS.  
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
garchmat=zeros(k,1+p+q);
options=optimset('fmincon');
options=optimset(options,'TolCon',1e-3,'Display','off','Diagnostics','off','LargeScale','off','LevenbergMarquardt','on');
for i=1:k
    temparam=fattailed_garch(data(:,i),p,q,'NORMAL',[],options);
    garchmat(i,:)=temparam';
end

A=mean(garchmat(:,2:p+1));
B=mean(garchmat(:,p+2:p+q+1));


C=cov(data);
alpha0=sqrt(A);
beta0=sqrt(B);

StartC=C*(1-sum(alpha0.^2)-sum(beta0.^2));
CChol=chol(StartC)';
warning off %#ok<WNOFF>
startingparameters=[vech(CChol);alpha0;beta0];

k2=k*(k+1)/2;
if nargin<=3 || isempty(BEKKoptions)
    options=optimset('fminunc');
    options.Display='iter';
    options.Diagnostics='on';
else
    options=BEKKoptions;
end

parameters=fminunc('scalar_bekk_mvgarch_likelihood',startingparameters,options,data,p,q,k,k2,t);
[loglikelihood,likelihoods,Ht]=scalar_bekk_mvgarch_likelihood(parameters,data,p,q,k,k2,t);
loglikelihood=-loglikelihood;
likelihoods=-likelihoods;

stdresid=zeros(size(data));
for i=1:t
    stdresid(i,:)=data(i,:)*Ht(:,:,i)^(-0.5);
end


%Std Errors
if nargout>=6
    %Std Errors
    A=hessian_2sided('scalar_bekk_mvgarch_likelihood',parameters,data,p,q,k,k2,t);
    
    h=max(abs(parameters/2),1e-2)*eps^(1/3);
    hplus=parameters+h;
    hminus=parameters-h;
    likelihoodsplus=zeros(t,length(parameters));
    likelihoodsminus=zeros(t,length(parameters));
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hplus(i);
        [HOLDER, indivlike] = scalar_bekk_mvgarch_likelihood(hparameters,data,p,q,k,k2,t);
        likelihoodsplus(:,i)=indivlike;
    end
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hminus(i);
        [HOLDER, indivlike] = scalar_bekk_mvgarch_likelihood(hparameters,data,p,q,k,k2,t);
        likelihoodsminus(:,i)=indivlike;
    end
    scores=(likelihoodsplus-likelihoodsminus)./(2*repmat(h',t,1));
    B=cov(scores);
    A=A/t;
    stderrors=A^(-1)*B*A^(-1)*t^(-1);
end
