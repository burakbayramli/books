function [parameters, likelihood, stderrors, robustSE, ht, scores]=egarchX(data,p,o,q,errors, X,options, startingvals);
% PURPOSE:
%     EGARCHX(P,Q) parameter estimation with different error distributions, the Normal, The T, 
%     and the Generalized Error Distribution
% 
% USAGE:
%     [parameters, likelihood, stderrors, robustSE, ht, scores]=egarchX(data,p,o,q,errors, options, startingvals);
% 
% INPUTS:
%     data: A single column of zero mean random data, normal or not for quasi likelihood
% 
%     P: Non-negative, scalar integer representing a model order of the ARCH 
%        process
%
%     O: Number of assymetric terms to include
% 
%     Q: Positive, scalar integer representing a model order of the GARCH 
%        process: Q is the number of lags of the lagged conditional variances included
%        Can be empty([]) for ARCH process
% 
%     error:  The type of error being assumed, valid types are:
%             'NORMAL' - Gaussian Innovations
%             'STUDENTST' - T-distributed errors
%             'GED' - General Error Distribution
% 
% 
%     startingvals: A (1+p+q) (plus 1 if STUDENTT OR GED is selected for the nu parameter) vector of starting vals.
%        If you do not provide, a naieve guess of 1/(2*max(p,q)+1) is used for the arch and garch parameters,
%        and omega is set to make the real unconditional variance equal
%        to the garch expectation of the expectation.
% 
%     options: default options are below.  You can provide an options vector.  See HELP OPTIMSET
% 
% 
% OUTPUTS:
%     parameters : a [1+2*p+q X 1] column of parameters with omega, alpha1, alpha2, ..., alpha(p), absolute alpha(1), 
%              absolute alpha(2), ... , absolute alpha(p), beta1, beta2, ... beta(q)
% 
%     likelihood = the loglikelihood evaluated at he parameters
% 
%     robustSE = QuasiLikelihood std errors which are robust to some forms of misspecification(see White 94)
% 
%     stderrors = the inverse analytical hessian, not for quasi maximum liklihood
% 
%     ht = the estimated time varying VARIANCES
% 
%     scores = The numberical scores(# fo params by t) for M testing   
% 
% 
% COMMENTS:
%   EGARCH(P,Q) the following(wrong) constratins are used(they are right for the (1,1) case or any Arch case
%     (1) nu>2 of Students T and nu>1 for GED
%
%   The time-conditional variance, H(t), of a EGARCH(P,Q) process is modeled 
%   as follows:
%
%     log h(t) = Omega + Alpha(1)*r_{t-1}/(sqrt(h(t-1))) + Alpha(2)*r_{t-2}/(sqrt(h(t-2))) +...
%                    + Alpha(P)*r_{t-p}/(sqrt(h(t-p)))+ Absolute Alpha(1)* abs(r_{t-1}/(sqrt(h(t-1)))) + ...
%                    + Absolute Alpha(P)* abs(r_{t-p}/(sqrt(h(t-p)))) +  Beta(1)* log(H(t-1))
%                    + Beta(2)*log(H(t-2))+...+ Beta(Q)*log(H(t-q))
%
%   Default Options
%   
%   options  =  optimset('fmincon');
%   options  =  optimset(options , 'TolFun'      , 1e-003);
%   options  =  optimset(options , 'Display'     , 'iter');
%   options  =  optimset(options , 'Diagnostics' , 'on');
%   options  =  optimset(options , 'LargeScale'  , 'off');
%   options  =  optimset(options , 'MaxFunEvals' , '400*numberOfVariables');
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
t=size(data,1);
warning off

if strcmp(errors,'NORMAL') | strcmp(errors,'STUDENTST') | strcmp(errors,'GED')
    if strcmp(errors,'NORMAL') 
        errortype = 1;
    elseif strcmp(errors,'STUDENTST') 
        errortype = 2;
    else
        errortype = 3;
    end
else
    error('error must be one of the three strings NORMAL, STUDENTST, or GED');
end


if size(data,2) > 1
    error('Data series must be a column vector.')
elseif isempty(data)
    error('Data Series is Empty.')
end


if (length(q) > 1) | any(q < 0)
    error('Q must ba a single positive scalar or an empty vector for ARCH.')
end

if (length(p) > 1) | any(p <  0)
    error('P must be a single positive number.')
elseif isempty(p)
    error('P is empty.')
end

if isempty(q) | q==0;
    q=0;
    m=p;
else
    m  =  max(p,q);   
end

if nargin<7 | isempty(options)
    options  =  optimset('fmincon');
    options  =  optimset(options , 'TolFun'      , 1e-004);
    options  =  optimset(options , 'Display'     , 'iter');
    options  =  optimset(options , 'Diagnostics' , 'on');
    options  =  optimset(options , 'LargeScale'  , 'off');
    options  =  optimset(options , 'MaxFunEvals' , 400*(2+p+q)) ;
end


if nargin<8
    options2=optimset('fmincon');
    options2  =  optimset(options2 , 'TolFun'      , 1e-004);
    options2  =  optimset(options2 , 'Display'     , 'off');
    options2  =  optimset(options2 , 'Diagnostics' , 'off');
    options2  =  optimset(options2 , 'LargeScale'  , 'off');
    options2  =  optimset(options2 , 'MaxFunEvals' , 400*(2+p+q)) ;
 
    starters=egarch(data,p,o,q,errors, options2);
    alpha  =  starters(2:1+p+o);
    beta   =  starters(2+p+o:1+p+o+q);
    omega  =  starters(1);
    Xparam=zeros(size(X,2));
    if strcmp(errors,'STUDENTST')
        nu  = starters(length(starters));
    elseif strcmp(errors,'GED')
        nu  = starters(length(starters));
    else
        nu=[];
    end
else
    omega=startingvals(1);
    alpha=startingvals(2:p+1);
    talpha=startingvals(p+2:p+1+o);
    beta=startingvals(p+o+2:p+o+q+1);
    Xparam=startingvals(p+o+3);
end
LB         =  [];     
UB         =  [];     



if errortype == 1
    startingvals = [omega ; alpha ; talpha; beta;Xparam];
else
    startingvals = [omega ; alpha ; talpha; beta;Xparam; nu];
end


stdEstimate =  std(data,1);                      
data        =  [stdEstimate(ones(m,1)) ; data];  
X=[zeros(size(X,2),m);X];

T           =  size(data,1);                    
[parameters, LLF,EXITFLAG,OUTPUT,GRAD] =  fminunc('egarchXEstLikelihood', startingvals ,options, data, p ,o, q, T, X,stdEstimate, errortype);
%[parameters, LLF, EXITFLAG, OUTPUT, LAMBDA, GRAD] =  fmincon('egarchEstLikelihood', startingvals ,sumA  , sumB ,[] , [] , LB , UB,[],options, data, p ,o, q, T, stdEstimate, errortype);

if EXITFLAG<=0
    EXITFLAG
    fprintf(1,'Not Sucessful! \n')
end

[likelihood, ht]=egarchXlikelihood(parameters,data,p,o,q,T,X,stdEstimate,errortype);

hess = hessian_2sided('egarchXlikelihood',parameters,data,p,o,q,T,X,stdEstimate,errortype);
likelihood=-likelihood;
stderrors=hess^(-1);

if nargout > 4
    h=min(abs(parameters/2),max(parameters,1e-2))*eps^(1/3);
    hplus=parameters+h;
    hminus=parameters-h;
    likelihoodsplus=zeros(t,length(parameters));
    likelihoodsminus=zeros(t,length(parameters));
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hplus(i);
        [HOLDER, HOLDER1, indivlike] = egarchXlikelihood(hparameters,data,p,o,q,T,X,stdEstimate,errortype);
        likelihoodsplus(:,i)=indivlike;
    end
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hminus(i);
        [HOLDER, HOLDER1, indivlike] = egarchXlikelihood(hparameters,data,p,o,q,T,X,stdEstimate,errortype);
        likelihoodsminus(:,i)=indivlike;
    end
    scores=(likelihoodsplus-likelihoodsminus)./(2*repmat(h',t,1));
    scores=scores-repmat(mean(scores),t,1);
    B=scores'*scores;
    robustSE=stderrors*B*stderrors;
end
warning on