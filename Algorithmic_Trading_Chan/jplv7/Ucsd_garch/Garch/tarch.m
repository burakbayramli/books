function [parameters, likelihood, ht, stderrors, robustSE,  scores] = tarch(data , p , o, q , type, startingvals, options)
% PURPOSE:
%     TARCH(P,O,Q) parameter estimation with normal innovations using analytic derivatives
% 
% USAGE:
%     [parameters, likelihood, ht, stderrors, robustSE,  scores] = tarch(data , p , o, q , type, startingvals, options)
% 
% INPUTS:
%     data: A single column of zero mean random data, normal or not for quasi likelihood
%  
%     P: Positive, scalar integer representing a model order of the ARCH 
%       process
% 
%     O: Order of tarch terms to include
%
%     Q: Non-negative, scalar integer representing a model order of the GARCH 
%       process: Q is the number of lags of the lagged conditional variances included
%       Can be empty([]) for ARCH process
% 
%     type:(optional) Either 'TARCH' or 'GJR'  Tarch works on std devs and GJR on variances. Default is TARCH
%
%     startingvals: A (1+p+q) vector of starting vals.  If you do not provide, a naieve guess is
%       used for the arch and garch parameters, and omega is set to make the real unconditional variance equal
%       to the garch expectation of the expectation.
% 
%     options: default options are below.  You can provide an options vector.  See HELP OPTIMSET
% 
% OUTPUTS:
%     parameters : a [1+p+o+q X 1] column of parameters with omega, alpha1, alpha2, ..., alpha(p)
%                 tarchp(1), ... , tarchp(o), beta1, beta2, ... beta(q)
% 
%     likelihood = the loglikelihood evaluated at he parameters
% 
%     ht = the estimated time varying VARIANCES
% 
%     stderrors = the inverse analytical hessian, not for quasi maximum liklihood
% 
%     robustSE = robust standard errors of form A^-1*B*A^-1*T^-1
%               where A is the analytic hessian
%               and B is the covariance of the scores
% 
%     scores = the list of T scores for use in M testing
% 
% COMMENTS:
% 
%   GARCH(P,Q) the following(wrong) constratins are used(they are right for the (1,1) case or any Arch case
%     (1) Omega > 0
%     (2) Alpha(i) >= 0 for i = 1,2,...P
%     (3) Beta(i)  >= 0 for i = 1,2,...Q
%     (4) sum(Alpha(i) + Beta(j)) < 1 for i = 1,2,...P and j = 1,2,...Q
% 
%   The time-conditional variance, H(t), of a GARCH(P,Q) process is modeled 
%   as follows:
% 
%     H(t) = Omega + Alpha(1)*r_{t-1}^2 + Alpha(2)*r_{t-2}^2 +...+ Alpha(P)*r_{t-p}^2+...
%                    Beta(1)*H(t-1)+ Beta(2)*H(t-2)+...+ Beta(Q)*H(t-q)
% 
%   Default Options
%   
%   options  =  optimset('fmincon');
%   options  =  optimset(options , 'TolFun'      , 1e-003);
%   options  =  optimset(options , 'Display'     , 'iter');
%   options  =  optimset(options , 'Diagnostics' , 'on');
%   options  =  optimset(options , 'LargeScale'  , 'off');
%   options  =  optimset(options , 'MaxFunEvals' , '400*numberOfVariables');
%   options  =  optimset(options , 'GradObj'     , 'on');
% 
% 
%  uses TARCH_LIKELIHOOD and TARCHCORE or GJRCORE.  You should MEX, mex 'path\garchcore.c', the MEX source 
%  The included MEX is for R12, 12.1 and 11 Windows and was compiled with Intel Compiler 5.01.
%  It gives a 10-15 times speed increase
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

if size(data,2) > 1
    error('Data series must be a column vector.')
elseif isempty(data)
    error('Data Series is Empty.')
end


if (length(q) > 1) | any(q < 0)
    error('Q must ba a single positive scalar or an empty vector for ARCH.')
end

if (length(o) > 1) | any(o <  0)
    error('O must be a single positive number.')
end

if (length(p) > 1) | any(p <  0)
    error('P must be a single positive number.')
elseif isempty(p)
    error('P is empty.')
end

if isempty(q)
    q=0;
    m=p;
else
    m  =  max(p,q);   
end

if nargin<=4 | isempty(type) | strcmp(type,'TARCH')
    type=1;
elseif strcmp(type,'GJR')
    type=2;
else 
    error('Can onyl do TARCH and GJR')
end

if nargin<=6 | isempty(startingvals)
    alpha  =  .1*ones(p,1)/p;
    tarch  =  .1*ones(o,1)/o;
    beta   =  .80*ones(q,1)/q;
    omega  =  0.1*cov(data);  %set the uncond = to its expection
else
    omega=startingvals(1);
    alpha=startingvals(2:p+1);
    tarchstartingvals(p+2:p+o+1);
    beta=startingvals(p+o+2:p+q+o+1);
end


LB         =  [];     
UB         =  [];     
sumA =  [-eye(1+p+q+o); ...
        0  ones(1,p) 0.5*ones(1,o) ones(1,q)];
sumB =  [zeros(1+p+q+o,1);...
        1];                

if (nargin <= 6) | isempty(options)
    options  =  optimset('fmincon');
    options  =  optimset(options , 'TolFun'      , 1e-003);
    options  =  optimset(options , 'Display'     , 'iter');
    options  =  optimset(options , 'Diagnostics' , 'on');
    options  =  optimset(options , 'LargeScale'  , 'off');
    options  =  optimset(options , 'MaxFunEvals' , 400*(1+p+q));
    options  =  optimset(options , 'GradObj'     , 'off');
end
sumB = sumB - [zeros(1+p+q+o,1); 1]*2*optimget(options, 'TolCon', 1e-6);


stdEstimate =  std(data,1);  
t=length(data);
data        =  [stdEstimate(ones(m,1)) ; data];
% Estimate the parameters.
[parameters, LLF, EXITFLAG, OUTPUT, LAMBDA, GRAD] =  fmincon('tarchlikelihood', [omega ; alpha; tarch ; beta] ,sumA  , sumB ,[] , [] , LB , UB,[],options,data, p ,o, q, m, stdEstimate, type);

if EXITFLAG<=0
    EXITFLAG
    fprintf(1,'Not Sucessful! \n')
end

parameters(find(parameters<0)) = 0;
parameters(find(parameters(1) <= 0)) = realmin;

[likelihood, ht]=tarchlikelihood(parameters,data, p ,o, q, m, stdEstimate, type);
likelihood=-likelihood;

%Calculate std errors if needed
if nargout >= 3
    hess = hessian_2sided('tarchlikelihood',parameters,data, p ,o, q, m, stdEstimate, type);
    stderrors=hess^(-1);
    h=min(abs(parameters/2+1e4),max(parameters,1e-2))*eps^(1/3);
    hplus=parameters+h;
    hminus=parameters-h;
    likelihoodsplus=zeros(t,length(parameters));
    likelihoodsminus=zeros(t,length(parameters));
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hplus(i);
        [HOLDER, HOLDER1, indivlike] = tarchlikelihood(hparameters,data, p ,o, q, m, stdEstimate, type);
        likelihoodsplus(:,i)=indivlike;
    end
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hminus(i);
        [HOLDER, HOLDER1, indivlike] = tarchlikelihood(hparameters,data, p ,o, q, m, stdEstimate, type);
        likelihoodsminus(:,i)=indivlike;
    end
    scores=(likelihoodsplus-likelihoodsminus)./(2*repmat(h',t,1));
    scores=scores-repmat(mean(scores),t,1);
    B=scores'*scores;
    robustSE=stderrors*B*stderrors;
end