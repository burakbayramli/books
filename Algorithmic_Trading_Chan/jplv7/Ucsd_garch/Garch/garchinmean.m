function [parameters, LLF, ht, errors, SEregression, stderrors, robustSE, scores, likelihoods] = garchinmean(data,ar,ma,p,q, options)
% PURPOSE:
%     Function designed to provide ARMA-Garch-in-Mean estimation
% 
% USAGE:
%     [parameters, LLF, h, errors, SEregression, stderrors, robustSE, scores, likelihoods] = garchinmean(data,ar,ma,p,q, options)
% 
% INPUTS:
%     data     - Dependant Time-Series vairable [T by 1]
%     ar       - the AR order desired
%     ma       - the MA order desired
%     p        - The number of ARCH componets fo the model
%     q        - The number of GARCH componets fo the model
%     options  - [optional] Options to use for the optimization, a fmincon process
% 
% OUTPUTS:
%     parameters   - an outputvector consisting of the following, omit anything you don't ask for:
%                  [CONSTANT AR MA GATCHCONST ARCH GARCH]
%     LLF          - The log-likelihood of the regression
%     h            - Time series of conditional volatility
%     errors       - A T-ar by 1 length vector of errors from the regression
%     SEregression - The standard errors of the regressions
%     stderrors    - The estimated variance covariance matrix of the parameters
%     robustSE     - The White robust Variance Covaiance matrix for incorrect specification of the errors
%     scores       - A T-ar by #params  matrix fo scores
%     likelihoods  - A T-ar by 1 vector of the individual log likelihoods for each obs
% 
% COMMENTS:
%     uses GARCHINMEANLIKELIHOOD and GARCHINMEANCORE.  You should MEX, mex 'path\garchinmeancore.c', the MEX source 
%     The included MEX is for R12 Windows and was compiled with Intel Compiler. It gives a 20-30 times speed increase.
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2.1    Date: 6/19/2001
% Fixed cases where ar=ma=0 or any of the above


m=max([ar ma p q]);
%how to get starting values - Clever Way
% Estimate Arma to get errors, estimate garch to get ht, then estimate ARMAX to get approx garch in mean
options2  =  optimset('fmincon');
options2  =  optimset(options2 , 'TolFun'      , 1e-006);
options2  =  optimset(options2 , 'Display'     , 'off');
options2  =  optimset(options2 , 'Diagnostics' , 'off');
options2  =  optimset(options2 , 'LargeScale'  , 'off');
options2  =  optimset(options2 , 'MaxFunEvals' , 400*(2+p+q));

if ~(ar==0 & ma==0)
    [parameters, errors]=armaxfilter(data,1,ar,ma,[],options2);
    [gparameters, likelihood, stderrors, robustSE, ht] = fattailed_garch(errors , p , q , 'NORMAL', [] , options2);
    ht=[ones(length(data)-length(ht),1)*cov(data); ht];
    [parameters]=armaxfilter(data,1,ar,ma,ht.^(0.5),options2);
    startparam=[parameters gparameters'];
else
    parameters=mean(data);
    errors=data-mean(data);
    [gparameters, likelihood, stderrors, robustSE, ht] = fattailed_garch(errors , p , q , 'NORMAL', [] , options2);
    [parameters]=[ones(size(ht)) ht.^(0.5)]\data;
    startparam=[parameters' gparameters'];
end


%Need to set up the regressandand the regressors here
if ar==0
    regressand=data;
    regressors=ones(size(data));
else
    [regressand,regressors]=newlagmatrix(data,ar,1);
end


% What constraints to use?  
% Arma Const unconstrainted, |ar|<1, |ma|<1, garchim no constraint.
% Garch, sum a+b<1  a,b,w>0
s=length(parameters);

% Build up the A matrix and teh B Matrix, first the AR part
A1=[zeros(ar,1) eye(ar) zeros(ar, s-1-ar); zeros(ar,1) -eye(ar) zeros(ar, s-1-ar)];
B1=[ones(2*ar,1)];
% Then the MA part
A2=[zeros(ma,1+ar) eye(ma) zeros(ma, s-1-ar-ma); zeros(ma,1+ar) -eye(ma) zeros(ma, s-1-ar-ma)];
B2=[ones(2*ma,1)];
if ma==0
    A2=[];
    B2=[];
end
% Now for the garch parameters >0 + sum<1
A3=[-eye(length(gparameters));0 ones(1,length(gparameters)-1)];
B3=[zeros(size(A3,1)-1,1); 1];

% Finally put it all together
% Need right padding for the mean and left for the GARCH
A=[A1;A2];
B=[B1;B2];
A=[A zeros(size(A,1),size(A3,2)); zeros(size(A3,1),size(A,2)) A3];
B=[B;B3];


k=size(regressors,2);
stdEstimate=std(regressand);
regressand=[zeros(m,1);regressand];
regressors=[zeros(m,k);regressors];
T=length(regressand);


if nargin<6
options  =  optimset('fmincon');
options  =  optimset(options , 'TolFun'      , 1e-006);
options  =  optimset(options , 'Display'     , 'iter');
options  =  optimset(options , 'Diagnostics' , 'on');
options  =  optimset(options , 'LargeScale'  , 'off');
options  =  optimset(options , 'MaxFunEvals' , 400*(2+p+q));
end

warning off;
parameters=fmincon('garchinmeanlikelihood',startparam',A,B,[],[],[],[],[],options,regressand, regressors, ar, ma, p, q, m ,T,stdEstimate);


[LLF, errors, likelihoods, ht]=garchinmeanlikelihood(parameters,regressand, regressors, ar, ma, p, q, m ,T,stdEstimate);
LLF=-LLF;
SEregression=sqrt(errors'*errors/(length(errors)-length(parameters)));

if nargout >=4
    stderrors=hessian_2sided('garchinmeanlikelihood',parameters,regressand, regressors, ar, ma, p, q, m ,T,stdEstimate);
    stderrors=stderrors^(-1);
end
%Make the parameters into the correct form, Constant, AR, MA, X

if nargout > 5
    t=length(regressand)-m;
    h=max(abs(parameters/2),1e-2)*eps^(1/3);
    hplus=parameters+h;
    hminus=parameters-h;
    likelihoodsplus=zeros(t,length(parameters));
    likelihoodsminus=zeros(t,length(parameters));
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hplus(i);
        [HOLDER, HOLDER1, indivlike] = garchinmeanlikelihood(hparameters,regressand, regressors, ar, ma, p, q, m ,T,stdEstimate);
        likelihoodsplus(:,i)=indivlike;
    end
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hminus(i);
        [HOLDER, HOLDER1, indivlike] = garchinmeanlikelihood(hparameters,regressand, regressors, ar, ma, p, q, m ,T,stdEstimate);
        likelihoodsminus(:,i)=indivlike;
    end
    scores=(likelihoodsplus-likelihoodsminus)./(2*repmat(h',t,1));
    scores=scores-repmat(mean(scores),t,1);
    B=scores'*scores;
    robustSE=stderrors*B*stderrors;
end

