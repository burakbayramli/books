function [parameters, loglikelihood, R ,Ht, likelihoods, stdresid, unistdresid, hmat, stderrors, A, B, jointscores]=cc_mvgarch(data,archP,garchQ)
% PURPOSE:
%        Estimates a multivariate GARCH model using Bollerslev's constant correlation estimator 
% 
% USAGE:
%      [parameters, loglikelihood, R, Ht, likelihoods, stdresid, unistdresid, hmat, stderrors, A, B, jointscores]=...
%                cc_mvgarch(data,archP,garchQ)
% 
% INPUTS:
%      data:   A zero mean t by k vector of residuals from some filtration
%      archP:  One of three things:    Empty   in which case a 1 innovation model is estimated for each series
%                                      A scalar, p     in which case a p innovation model is estimated for each series
%                                      A k by 1 vector in which case the ith series has innovation terms p=archP(i)
%      garchQ:  One of three things:    Empty   in which case a 1 GARCH lag is used in estimation for each series
%                                      A scalar, q     in which case a q GARCH lags is used in estimation for each series
%                                      A k by 1 vector in which case the ith series has lagged variance terms q=archQ(i)
% 
% OUTPUTS:
%      parameters= A vector of parameters estimated form the model of the form
%                  [GarchParams(1) GarchParams(2) ... GarchParams(k) Correlation(ccvech of the correlation matrix)]
%                  where the garch parameters from each estimation are of the form
%                  [omega(i) alpha(i1) alpha(i2) ... alpha(ip(i)) beta(i1) beta(i2) ... beta(iq(i))]
%      loglikelihood=The log likelihood evaluated at the optimum
%      R = k x k matrix of correlations
%      Ht= A k by k by t array of conditional variances
%      likelihoods = the estimated likelihoods t by 1
%      stdresid = The multivariate standardized residuals
%      unistdresid = Residuals standardized by their estimated std devs
%      Hmat = The t by k matrix of conditional variances
%      stderrors=A length(parameters)^2 matrix of estimated correct standard errors
%      A = The estimated A form the rebust standard errors
%      B =the estimated B from the standard errors
%      scores = The estimated scores of the likelihood t by length(parameters)
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
    hmat=zeros(t,k);
for i=1:k
    fprintf(1,'Estimating GARCH model for Series %d\n',i)
    [univariate{i}.parameters, univariate{i}.likelihood, univariate{i}.stderrors, univariate{i}.robustSE, univariate{i}.ht, univariate{i}.scores] ... 
        = fattailed_garch(data(:,i) , archP(i) , garchQ(i) , 'NORMAL',[], options);
    stdresid(:,i)=data(:,i)./sqrt(univariate{i}.ht);
    hmat(:,i)=univariate{i}.ht;
end
unistdresid=stdresid;
% The estimated parameters are real easy
R=corrcoef(stdresid);


% We now have all of the estimated parameters
parameters=[];
H=zeros(t,k);
for i=1:k
    parameters=[parameters;univariate{i}.parameters];
    H(:,i)=univariate{i}.ht;
end
parameters=[parameters;ccvech(R)];


%We now have Ht and the likelihood
if nargout >=2
    [loglikelihood, likelihoods]=cc_mvgarch_full_likelihood(parameters, data, archP,garchQ);
    likelihoods=-likelihoods;
    loglikelihood=-loglikelihood;
    Ht=zeros(k,k,t);
    stdresid=zeros(t,k);
    Hstd=H.^(0.5);
    for i=1:t
        Ht(:,:,i)=diag(Hstd(i,:))*R*diag(Hstd(i,:));
        stdresid(i,:)=data(i,:)*Ht(:,:,i)^(-0.5);
    end
    
    if nargout>=9
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
        otherA=dcc_hessian('cc_mvgarch_full_likelihood',parameters, (k*(k-1)/2), data, archP,garchQ);
        A(length(parameters)-(k*(k-1)/2)+1:length(parameters),:)=otherA;
        % tempA=hessian_2sided('dcc_garch_full_likelihood',parameters, data, archP,garchQ,dccP,dccQ);
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
        for i=length(parameters)-(k*(k-1)/2)+1:length(parameters)
            hparameters=parameters;
            hparameters(i)=hplus(i);
            [HOLDER, indivlike] = cc_mvgarch_full_likelihood(hparameters, data, archP,garchQ);
            likelihoodsplus(:,i)=indivlike;
        end
        for i=length(parameters)-(k*(k-1)/2)+1:length(parameters)
            hparameters=parameters;
            hparameters(i)=hminus(i);
            [HOLDER, indivlike] = cc_mvgarch_full_likelihood(hparameters, data, archP,garchQ);
            likelihoodsminus(:,i)=indivlike;
        end
        CCscores=(likelihoodsplus(:,length(parameters)-(k*(k-1)/2)+1:length(parameters))-likelihoodsminus(:,length(parameters)-(k*(k-1)/2)+1:length(parameters)))...
            ./(2*repmat(h(length(parameters)-(k*(k-1)/2)+1:length(parameters))',t,1));
        jointscores(:,length(parameters)-(k*(k-1)/2)+1:length(parameters))=CCscores;
        B=cov(jointscores);
        stderrors=A^(-1)*B*A^(-1)*t;
    end
    %Done!
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [parameters]=ccvech(CorrMat)
[k,t]=size(CorrMat);

parameters=zeros(k*(k-1)/2,1);
index=1;
for i=1:k
    for j=i+1:k
        parameters(index)=CorrMat(i,j);
        index=index+1;
    end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [CorrMat]=ccivech(params)
[k,t]=size(params);
for i=2:m
    if (k/((i*(i-1))/2))==1
        sizes=i;
        break
    end
end

index=1;
CorrMat=eye(sizes)
for i=1:sizes
    for j=i+1:sizes
        CorrMat(i,j)=params(index);
        CorrMat(j,i)=params(index);
        index=index+1;
    end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
