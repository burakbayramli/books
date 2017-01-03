function [parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores] = diagonal_bekk_T_mvgarch(data,p,q,BEKKoptions);
% PURPOSE:
%      To Estimate a diagonal BEKK multivariate GARCH model with T-dist errors.  ****SEE WARNING AT END OF HELP FILE****
% 
% USAGE:
%      [parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores]  = diagonal_bekk_T_mvgarch(data,p,q,options);
% 
% INPUTS:
%      data          - A t by k matrix of zero mean residuals
%      p             - The lag length of the innovation process
%      q             - The lag length of the AR process
%      options       - (optional) Options for the optimization(fminunc)
% 
% OUTPUTS:
%      parameters    - A (k*(k+1))/2+p*k+q*k+1 vector of estimated parameteters. 
%                         For any k set of Innovation or AR parameters X, 
%                         diag(X) will give the correct matrix
%                         To recover C, use ivech(parmaeters(1:(k*(k+1))/2), nu is the last parameter
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
%      *  THIS FUNCTION INVOLVES ESTIMATING QUITE A FEW PARAMETERS.  THE EXACT NUMBER OF 
%      *  PARAMETERS NEEDING TO BE ESTIMATED IS (k*(k+1))/2+pk+qk.  FOR A 5 VARIATE (1,1) MODEL 
%      *  THIS INVLOVES 25 PARAMETERS.  IN ADDITION, IT ESTIMATES A SCALAR_BEKK_MVGARCH 
%      *  MODEL FOR STARTING VALUES.
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
scalaropt=optimset(scalaropt,'TolX',1e-2,'TolFun',1e-1,'Display','iter','Diagnostics','on','DiffMaxChange',1e-2);
startingparameters=scalar_bekk_mvgarch(data,p,q,scalaropt);
CChol=startingparameters(1:(k*(k+1))/2);
C=ivech(startingparameters(1:(k*(k+1))/2))*ivech(startingparameters(1:(k*(k+1))/2))';
newA=[];
newB=[];
for i=1:p
    newA=[newA; (ones(k,1))*startingparameters(((k*(k+1))/2)+i)]  ;  
end
for i=1:q
    newB=[newB;(ones(k,1))*startingparameters(((k*(k+1))/2)+i+p)];
end
startingparameters=[CChol;newA;newB];


if nargin<=3 | isempty(BEKKoptions)
    options=optimset('fminunc');
    options.Display='iter';
    options.Diagnostics='on';
    options.MaxFunEvals=500*length(startingparameters);   
else
    options=BEKKoptions;
end
startingparameters=[startingparameters;3];
parameters=fminunc('diagonal_bekk_T_est_likelihood',startingparameters,options,data,p,q,k,k2,t);
parameters(length(parameters))=2.1+parameters(length(parameters))^2;
[loglikelihood,likelihoods,Ht]=diagonal_bekk_T_likelihood(parameters,data,p,q,k,k2,t);
loglikelihood=-loglikelihood;
likelihoods=-likelihoods;

% Standardized residuals
stdresid=zeros(size(data));
for i=1:t
    stdresid(i,:)=data(i,:)*Ht(:,:,i)^(-0.5);
end


%Std Errors
if nargout>=6
    A=hessian_2sided('diagonal_bekk_T_likelihood',parameters,data,p,q,k,k2,t);
    h=max(abs(parameters/2),1e-2)*eps^(1/3);
    hplus=parameters+h;
    hminus=parameters-h;
    likelihoodsplus=zeros(t,length(parameters));
    likelihoodsminus=zeros(t,length(parameters));
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hplus(i);
        [HOLDER, indivlike] = diagonal_bekk_T_likelihood(hparameters,data,p,q,k,k2,t);
        likelihoodsplus(:,i)=indivlike;
    end
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hminus(i);
        [HOLDER, indivlike] = diagonal_bekk_T_likelihood(hparameters,data,p,q,k,k2,t);
        likelihoodsminus(:,i)=indivlike;
    end
    scores=(likelihoodsplus-likelihoodsminus)./(2*repmat(h',t,1));
    B=cov(scores);
    A=A/t;
    stderrors=A^(-1)*B*A^(-1)*t^(-1);
end




