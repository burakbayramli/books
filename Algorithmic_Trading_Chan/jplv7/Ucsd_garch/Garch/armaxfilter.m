function [parameters, errors, LLF , SEregression,stderrors, robustSE,   scores, likelihoods]=armaxfilter(y,constant,ar,ma,x,options)
% PURPOSE:
%     Function designed to provide a good ARMAX time series filter
% 
% 
% USAGE:
%     [parameters, errors, LLF , SEregression, stderrors, robustSE, scores, likelihoods]=armaxfilter(y,constant,ar,ma,x,options)
% 
% 
% INPUTS:
%     y        - Dependant Time-Series vairable [T by 1]
%     constant - 1 or 0. 1 indicates the presece of a constant in the regression, 0 no constant
%     ar       - the AR order desired
%     ma       - the MA order desired
%     x        - [optional] a matrix of other exogenous variables.  These line up exactly with the Y's and if  
%            they are time series, you need to shift them down by 1 place,  i.e. pad the bottom with 1 
%            observation and cut off the top row [ T by K]
%     options  - [optional] Options to use for the optimization, a fmincon process
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
% 
% COMMENTS:
%     uses ARMAXFILTER_LIKELIHOOD and MAXCORE.  You should MEX, mex 'path\maxcore.c', the MEX source 
%     The included MEX is for R12 Windows and was compiled with Intel Compiler. It gives a 20-30 times speed increase.
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
if ~any([constant ar ma])
    error('At least one of CONST AR or MA must be nonnegative')
end

if any([constant ar ma]<0)
    error('CONST AR or MA cannot be negative')
end
if nargin>4 & ~isempty(x)
    if length(x)~=length(y)
        error('Data length mismatch between x and y')
    end
end

if nargin<4
    error('Requires at least 4 inputs')
end

T=length(y);
t=T-ar;
[regressand,lags]=newlagmatrix(y,ar,constant);

if ar~=0
    if nargin>4 & ~isempty(x) %ARX or ARMAX
        TempX=x(ar+1:T,:);
        regressors=[lags';TempX']';
    else % ARMA or AR
        regressors=lags;
        x=[];
    end
else
    if nargin>4 & ~isempty(x) % MAX
        TempX=x(ar+1:T,:);
        regressors=[lags';TempX']';
    else % Pure MA
        regressors=lags;
        x=[];
    end
end

% Use a few iterations of the zig-zag algorithm for starting values

d=1;
niter=0;
beta=regressors\regressand;
e=regressand-regressors*beta;
[e,elags]=newlagmatrix(e,ma,0);
delta=elags\e;
while d>1e-2 & niter<=2
    oldparams=[beta;delta];
    elags=[zeros(ma,size(elags,2));elags];
    newregressand=regressand-elags*delta;
    beta=regressors\newregressand;
    e=newregressand-regressors*beta;
    [e,elags]=newlagmatrix(e,ma,0);
    delta=elags\e;
    d=max(abs(oldparams-[beta;delta]));
    niter=niter+1;
end

if nargin <=5
    options=optimset('fmincon');
    options=optimset(options,'Display','iter','Diagnostics','on','LargeScale','off','MaxFunEvals',5000*(ma+ar),'MaxIter',1000*(ma+ar));
end

regressand=[zeros(ma,1);regressand];
regressors=[zeros(ma,size(regressors,2));regressors];
tau=length(regressand);

parameters=[beta;delta];
s=length(parameters);

if constant~=0
    constant=1;
else
    constant=0;
end

A=[zeros(ar,constant) eye(ar)  zeros(ar,size(x,2)) zeros(ar,ma);...
        zeros(ar,constant) -eye(ar)  zeros(ar,size(x,2)) zeros(ar,ma);...
        zeros(ma,constant) zeros(ma,ar) zeros(ma,size(x,2)) eye(ma);...
        zeros(ma,constant) zeros(ma,ar) zeros(ma,size(x,2)) -eye(ma)];
B=ones(size(A,1),1);

[parameters]=fmincon('armaxfilter_likelihood',parameters',A,B,[],[],[],[],[],options, regressand , regressors, ar , ma, tau);




[LLF,errors,likelihoods]=armaxfilter_likelihood(parameters,regressand , regressors, ar , ma, tau);
LLF=-LLF;
SEregression=sqrt(errors'*errors/(length(errors)-length(parameters)));
parameters=parameters';
if nargout >=4
    stderrors=hessian_2sided('armaxfilter_likelihood',parameters,regressand , regressors, ar , ma, tau);
    stderrors=stderrors^(-1);
end
%Make the parameters into the correct form, Constant, AR, MA, X
parameters=parameters';
if nargout > 5
    h=max(abs(parameters/2),1e-2)*eps^(1/3);
    hplus=parameters+h;
    hminus=parameters-h;
    likelihoodsplus=zeros(t,length(parameters));
    likelihoodsminus=zeros(t,length(parameters));
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hplus(i);
        [HOLDER, HOLDER1, indivlike] = armaxfilter_likelihood(hparameters,regressand , regressors, ar , ma, tau);
        likelihoodsplus(:,i)=indivlike;
    end
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hminus(i);
        [HOLDER, HOLDER1, indivlike] = armaxfilter_likelihood(hparameters,regressand , regressors, ar , ma, tau);
        likelihoodsminus(:,i)=indivlike;
    end
    
    scores=(likelihoodsplus-likelihoodsminus)./(2*repmat(h,t,1));
    scores=scores-repmat(mean(scores),t,1);
    B=scores'*scores;
    robustSE=stderrors*B*stderrors;
end
