function [parameters,  SEregression, errors, LLF, stderrors, robustSE, scores, likelihoods]=armaxfilter(y,constant,ma,x,options)
% PURPOSE:
%     Function designed to provide a good MAX time series filter
% 
% USAGE:
%     [parameters,  SEregression, errors, LLF, stderrors, robustSE, scores, likelihoods]=armaxfilter(y,constant,ma,x,options)
% 
% INPUTS:
%     y        - Dependant Time-Series vairable [T by 1]
%     constant - 1 or 0. 1 indicates the presece of a constant in the regression, 0 no constant
%     ma       - the MA order desired
%     x        - [optional] a matrix of other exogenous variables.  These line up exactly with the Y's and if  
%                they are time seriesm you need to shift them down by 1 place,  i.e. pad the bottom with 1 
%                observation and cut off the top row [ T by K]
%     options  - [optional] Options to use for the optimization, a fminunc process
% 
% 
% OUTPUTS:
%     parameters   - an outputvector consisting of the following, omit anything you don't ask for:
%                    [CONSTANT AR EXOGENOUS MA]
%     stderrors    - The estimated variance covariance matrix of the parameters
%     robustSE     - The White robust Variance Covaiance matrix for incorrect specification of the errors
%     SEregression - The standard errors of the regressions
%     errors       - A T-ar by 1 length vector of errors from the regression
%     LLF          - The log-likelihood of the regression
%     scores       - A T-ar by #params  matrix fo scores
%     likelihoods  - A T-ar by 1 vector of the individual log likelihoods for each obs
% 
% COMMENTS:
%     uses MAXFILTER_LIKELIHOOD and MAXCORE.  You should MEX, mex 'path\maxcore.c', the MEX source 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


T=length(y);
t=T;

%SOMETHING IS WRONG HERE
if constant==1 & nargin>4 & ~isempty(x)
    regressors=[ones(T,1) regressors];
    beta=regessors\y;
    e=y-beta'*regessors;
    [E,E1]=newlagmatrix(e,ma,0);
    delta=E1\E;
elseif constant==1 
    regressors=[ones(T,1)];
    beta=regressors\y;
    e=y-beta'*regressors;
    [E,E1]=newlagmatrix(e,ma,0);
    delta=E1\E;
    
else
    regressors=[];
    [E,E1]=newlagmatrix(y,ma,0);
    delta=E1\E;
end

if nargin <=4
    options=optimset('fmincon');
    options=optimset(options,'Display','iter','Diagnostics','on','LargeScale','off');
end

regressand=y;
regressand=[zeros(ma,1);regressand];
regressors=[zeros(ma,size(regressors,2)); regressors];
A=[zeros(size(delta,1),size(beta,1)) eye(size(delta,1));zeros(size(delta,1),size(beta,1)) -eye(size(delta,1))];
B=ones(size(A,1),1);


[parameters]=fmincon('maxfilter_likelihood',[beta' delta'],A,B,[],[],[],[],[],options, regressand , regressors, ma);
parameters=parameters;
 
 if nargout >1
     stderrors=hessian_2sided('maxfilter_likelihood',parameters,regressand,regressors, ma);
     stderrors=stderrors^(-1);
     [LLF,errors,likelihoods]=maxfilter_likelihood(parameters,regressand , regressors, ma);
     LLF=-LLF;
     SEregression=sqrt(errors'*errors/(length(errors)-length(parameters)));
 end
%Make the parameters into the correct form, Constant, AR, MA, X
 
 if nargout > 4
     h=max(abs(parameters/2),1e-2)*eps^(1/3);
     hplus=parameters+h;
     hminus=parameters-h;
     likelihoodsplus=zeros(t,length(parameters));
     likelihoodsminus=zeros(t,length(parameters));
     for i=1:length(parameters)
         hparameters=parameters;
         hparameters(i)=hplus(i);
         [HOLDER, HOLDER1, indivlike] = maxfilter_likelihood(hparameters,regressand , regressors,  ma);
         likelihoodsplus(:,i)=indivlike;
     end
    for i=1:length(parameters)
         hparameters=parameters;
         hparameters(i)=hminus(i);
         [HOLDER, HOLDER1, indivlike] = maxfilter_likelihood(hparameters,regressand , regressors , ma);
         likelihoodsminus(:,i)=indivlike;
     end
     scores=(likelihoodsplus-likelihoodsminus)./(2*repmat(h,t,1));
     scores=scores-repmat(mean(scores),t,1);
     B=scores'*scores;
     robustSE=stderrors*B*stderrors;
 end
