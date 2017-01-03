function [parameters, loglikelihood, Ht, Qt,  stdresid, likelihoods, stderrors, A,B, jointscores]=dcc_mvgarch(data,dccP,dccQ,archP,garchQ)
% PURPOSE:
%        Estimates a multivariate GARCH model using the DCC estimator of Engle and Sheppard
% 
% USAGE:
%        [parameters, loglikelihood, Ht, Qt,  likelihoods, stdresid, stderrors, A,B, jointscores]...
%                =dcc_mvgarch(data,dccP,dccQ,archP,garchQ)
% 
% INPUTS:
%      data          = A zero mean t by k vector of residuals from some filtration
%      dccP          = The lag length of the innovation term in the DCC estimator
%      dccQ          = The lag length of the lagged correlation matrices in the DCC estimator
%      archP         = One of three things:    Empty   in which case a 1 innovation model is estimated for each series
%                                      A scalar, p     in which case a p innovation model is estimated for each series
%                                      A k by 1 vector in which case the ith series has innovation terms p=archP(i)
%      garchQ        = One of three things:    Empty   in which case a 1 GARCH lag is used in estimation for each series
%                                      A scalar, q     in which case a q GARCH lags is used in estimation for each series
%                                      A k by 1 vector in which case the ith series has lagged variance terms q=archQ(i)
% 
% OUTPUTS:
%      parameters    = A vector of parameters estimated form the model of the form
%                          [GarchParams(1) GarchParams(2) ... GarchParams(k) DCCParams]
%                          where the garch parameters from each estimation are of the form
%                          [omega(i) alpha(i1) alpha(i2) ... alpha(ip(i)) beta(i1) beta(i2) ... beta(iq(i))]
%      loglikelihood = The log likelihood evaluated at the optimum
%      Ht            = A k by k by t array of conditional variances
%      Qt            = A k by k by t array of Qt elements
%      likelihoods   = the estimated likelihoods t by 1
%      stderrors     = A length(parameters)^2 matrix of estimated correct standard errors
%      A             = The estimated A form the rebust standard errors
%      B             = The estimated B from the standard errors
%      scores        = The estimated scores of the likelihood t by length(parameters)
% 
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

% Lets do some error checking and clean up

[t,k]=size(data);

if k<2
    error('Must have at least 2 data series')
end

if length(dccP)~=length(dccQ) | length(dccP)~=1
    error('dccP and dccQ must be scalars')
end

if ~(isempty(archP) | length(archP)==1 | length(archP)==k)
    error('Wrong size for archP')
end

if ~(isempty(garchQ) | length(garchQ)==1 | length(garchQ)==k)
    error('Wrong size for garchQ')
end

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


% Now lest do the univariate garching using fattailed_garch as it's faster then garchpq
stdresid=data;
options=optimset('fmincon');
options=optimset(options,'Display','off','Diagnostics','off','MaxFunEvals',1000*max(archP+garchQ+1),'MaxIter',1000*max(archP+garchQ+1),'LargeScale','off','MaxSQPIter',1000);
options  =  optimset(options , 'MaxSQPIter' , 1000);
for i=1:k
    fprintf(1,'Estimating GARCH model for Series %d\n',i)
    [univariate{i}.parameters, univariate{i}.likelihood, univariate{i}.stderrors, univariate{i}.robustSE, univariate{i}.ht, univariate{i}.scores] ... 
        = fattailed_garch(data(:,i) , archP(i) , garchQ(i) , 'NORMAL',[], options);
    stdresid(:,i)=data(:,i)./sqrt(univariate{i}.ht);
end


options=optimset('fmincon');
options  =  optimset(options , 'Display'     , 'iter');
options  =  optimset(options , 'Diagnostics' , 'on');
options  =  optimset(options , 'LevenbergMarquardt' , 'on');
options  =  optimset(options , 'LargeScale'  , 'off');


dccstarting=[ones(1,dccP)*.01/dccP ones(1,dccQ)*.97/dccQ];
fprintf(1,'\n\nEstimating the DCC model\n')

[dccparameters,dccllf,EXITFLAG,OUTPUT,LAMBDA,GRAD]=fmincon('dcc_mvgarch_likelihood',dccstarting,ones(size(dccstarting)),[1-2*options.TolCon],[],[],zeros(size(dccstarting))+2*options.TolCon,[],[],options,stdresid,dccP,dccQ);



% We now have all of the estimated parameters
parameters=[];
H=zeros(t,k);
for i=1:k
    parameters=[parameters;univariate{i}.parameters];
    H(:,i)=univariate{i}.ht;
end
parameters=[parameters;dccparameters'];



%We now have Ht and the likelihood
[loglikelihood, Rt, likelihoods, Qt]=dcc_mvgarch_full_likelihood(parameters, data, archP,garchQ,dccP,dccQ);
likelihoods=-likelihoods;
loglikelihood=-loglikelihood;
Ht=zeros(k,k,t);
stdresid=zeros(t,k);
Hstd=H.^(0.5);
for i=1:t
    Ht(:,:,i)=diag(Hstd(i,:))*Rt(:,:,i)*diag(Hstd(i,:));
    stdresid(i,:)=data(i,:)*Ht(:,:,i)^(-0.5);
end
save tempHt Ht
clear Ht

if nargout >=7
    %How ar we going to get STD errors?  Partitioned invers probably.  Well, we need to get the scores form the dcc model, the joint likelihood.
    %We then need to get A12 and A22 so we can have it all.  We also need to get A11 in the correct form.
    A=zeros(length(parameters),length(parameters));
    index=1;
    for i=1:k
        workingsize=size(univariate{i}.stderrors);
        A(index:index+workingsize-1,index:index+workingsize-1)=univariate{i}.stderrors^(-1);
        index=index+workingsize;
    end
    
    % Ok so much for a All and A12 and A22, as we have them all between whats above
    fprintf(1,'\n\nCalculating Standard Errors, this can take a while\n');
    otherA=dcc_hessian('dcc_mvgarch_full_likelihood',parameters, dccP+dccQ, data, archP,garchQ,dccP,dccQ);
    A(length(parameters)-dccP-dccQ+1:length(parameters),:)=otherA;
    % tempA=hessian('dcc_garch_full_likelihood',parameters, data, archP,garchQ,dccP,dccQ);
    % A(length(parameters)-1:length(parameters),:)=tempA(length(parameters)-1:length(parameters),:);
    %That finishes A
    
    % We now need to get the scores for the DCC estimator so we can finish B
    jointscores=zeros(t,length(parameters));
    index=1;
    for i=1:k
        workingsize=size(univariate{i}.scores,2);
        jointscores(:,index:index+workingsize-1)=univariate{i}.scores;
        index=index+workingsize;
    end
    
    %Now all we need to do is calculate the scores form teh dcc estimator and we have everything
    h=max(abs(parameters/2),1e-2)*eps^(1/3);
    hplus=parameters+h;
    hminus=parameters-h;
    likelihoodsplus=zeros(t,length(parameters));
    likelihoodsminus=zeros(t,length(parameters));
    for i=length(parameters)-dccP-dccQ+1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hplus(i);
        [HOLDER, HOLDER1, indivlike] = dcc_mvgarch_full_likelihood(hparameters, data, archP,garchQ,dccP,dccQ);
        likelihoodsplus(:,i)=indivlike;
    end
    for i=length(parameters)-dccP-dccQ+1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hminus(i);
        [HOLDER, HOLDER1, indivlike] = dcc_mvgarch_full_likelihood(hparameters, data, archP,garchQ,dccP,dccQ);
        likelihoodsminus(:,i)=indivlike;
    end
    DCCscores=(likelihoodsplus(:,length(parameters)-dccP-dccQ+1:length(parameters))-likelihoodsminus(:,length(parameters)-dccP-dccQ+1:length(parameters)))...
        ./(2*repmat(h(length(parameters)-dccP-dccQ+1:length(parameters))',t,1));
    jointscores(:,length(parameters)-dccP-dccQ+1:length(parameters))=DCCscores;
    B=cov(jointscores);
    A=A/t;
    stderrors=A^(-1)*B*A'^(-1)*t^(-1);
end
%Done!
load tempHt
