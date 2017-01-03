function [parameters, likelihood, stderrors, robustSE, ht, scores]=multigarch(data,p,o,q,type,errors,options,startingvals)
% PURPOSE:
%     This is a multi use univariate GARCH function which can estimate 
%     GARCH(you should use garchpq though), EGARCH(Nelson), Threshold GARCH(Zakoian), 
%     Absolute Value GARCH(Taylor/Schwert), Non-Linear Asymetric GARCH(Engle Ng), 
%     GJR-GARCH(G,J &R), Nonlinear GARCH(Higgins Bera),
%     and asymetric power GARCH(Ding Engle and Granger),
%     and a flexible garch whiel allows for non-linearities, threshold effects, 
%     news impact rotation and recentering of the news impact curve
% 
% USAGE:
%     [parameters, likelihood, stderrors, robustSE, ht, scores]=multigarch(data,p,q,type,errors,options, startingvals)
% 
% INPUTS:
%     data: Zero Mean series of regression residuals or other zero mean series
%     p :  The order of the ARCH(innovations) process
%     o :  The order of the TARCH process(only for thoses models with threshold effects)
%     q :  The order of the GARCH process
%     type :  A string telling the proc which type of model is to be estimated
%             Can be one of the following(note: ALL CAPS)
%                   Without assymetric terms
%             'GARCH'    -  Normal GARCH Model(see garchpq or fattailed_garch instead) 
%             'AVGARCH'  -  Absolute Value GARCH                   
%             'NGARCH'   -  Non-linear GARCH                       
%             'NAGARCH'  -  Non-Linear Asymetric GARCH             
%                   With assymetric terms
%             'EGARCH'   -  Exponential GARCH                      
%             'TGARCH'   -  Threshold GARCH                        
%             'GJRCARCH' -  GJR Representation of TARCH                    
%             'APGARCH'  -  Asymetric Power GARCH                  
%             'ALLGARCH' -  Asymetric Power GARCH with news impact centering parameter 
% 
% OUTPUTS:
%    parameters: a 1+p+q+special column vector of estimated model parameters, the size
%                of special depends on the model being estimated
%    logl: The log-likelihood of the likelihood function.
%    parameters : a [1+p+o+q X 1] column of parameters with omega, alpha1, alpha2, ..., alpha(p)
%                 tarch(1), tarch(2), ... tarch(o) beta1, beta2, ... beta(q)
%    likelihood = the loglikelihood evaluated at he parameters
%    ht = the estimated time varying VARIANCES
%    stderrors = the inverse analytical hessian, not for quasi maximum liklihood
%    robustSE = robust standard errors of form A^-1*B*A^-1*T^-1
%              where A is the analytic hessian
%              and B is the covariance of the scores
%    scores = the list of T scores for use in M testing
%    
%    the special parameter will contain estimates of the following, in this order
%    (non-estimated parameters are not reported), range is in paranthesis
% 
%    lambda(0, infty] -  this is the parameter which determines the power of sigma being estimaded(i.e. 2 for GARCH)
%    b(-infty, infty) -  The centering parameter for the effect of the new impact on volatility
% 
% 
% COMMENTS:
% 
%     This proceedure estimates conditional volatility of the following form:
% 
%     h(t)^(lambda)-1                                                 h(t-1)^(lambda)-1
%     ------------  = omega + a*h(t-1)^(lambda)+f(data(t-1))^(nu)+ b*-------------------
%          lambda                                                           lambda
% 
%     f(data(t))=abs(data(t)-b) - c*(data(t)-b)
% 
%     This program is in no small part influenced by the work of 
%     L. Hentschel J. of Empirical Finance 95
% 
% 
%     Author: Kevin Sheppard
%     kevin.sheppard@economics.ox.ac.uk
%     Revision: 2    Date: 12/31/2001

if o~=0
    if strcmp(type,'GARCH') | strcmp(type,'AVGARCH') | strcmp(type,'NGARCH') | strcmp(type,'NAGARCH') 
        error('Selected model does not allow for asymetric terms')
    end
end

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

if (length(p) > 1) | any(p <=  0)
    error('P must be a single positive number.')
elseif isempty(p)
    error('P is empty.')
end

if (length(o) > 1) | any(o <  0)
    error('O must be a single positive number.')
end

if (nargin <= 6) | isempty(options)
    options  =  optimset('fmincon');
    options  =  optimset(options , 'TolFun'      , 1e-004);
    options  =  optimset(options , 'Display'     , 'iter');
    options  =  optimset(options , 'Diagnostics' , 'on');
    options  =  optimset(options , 'LargeScale'  , 'off');
    options  =  optimset(options , 'MaxFunEvals' , 400*20);
    options  =  optimset(options , 'MaxSQPIter'  , 500);
end

if strcmp(type,'EGARCH');
    [parameters, likelihood, stderrors, robustSE, ht, scores]=egarch(data,p,o,q,errors,options);
    return
elseif strcmp(type,'GARCH')
    [parameters, likelihood, stderrors, robustSE, ht, scores]=fattailed_garch(data,p,q,errors,[],options);
    return
end

if isempty(q)
    q=0;
    m=p;
else
    m  =  max(p,q);   
end

if nargin<=7
    guess  =  1/(2*m+1);
    alpha  =  .05*ones(p,1)/p;
    tarchp  =  .1*ones(o,1)/o;
    beta   =  .8*ones(q,1)/q;
    omega  =  0.1*cov(data);  %set the uncond = to its expection
else
    omega=startingvals(1);
    alpha=startingvals(2:p+1);
    tarchp=startingvals(p+2:p+o+1);
    beta=startingvals(p+o+2:p+q+o+1);
end

startingvalues=[omega ; alpha; tarchp ; beta];

newoptions  =  optimset('fmincon');
newoptions  =  optimset(newoptions , 'TolFun'      , 1e-1);
newoptions  =  optimset(newoptions , 'Display'     , 'off');
newoptions  =  optimset(newoptions , 'Diagnostics' , 'off');
newoptions  =  optimset(newoptions , 'LargeScale'  , 'off');
newoptions  =  optimset(newoptions , 'MaxFunEvals' , 600*(p+o+q));
newoptions  =  optimset(newoptions , 'MaxSQPIter' , 500);
startingvalues(1:1+p+q+o)=tarch(data,p,o,q,'TARCH',[],newoptions);

[lambda, nu, b, garchtype, indicator]=multi_garch_paramsetup(type);
[sumA, sumB, startingvalues, LB, UB, garchtype]=multi_garch_constraints( startingvalues, p,o, q, data, type);

if strcmp(errors,'STUDENTST')
    sumA=[sumA';zeros(1,size(sumA,1))]';
    nuconst=zeros(1,size(sumA,2));
    nuconst(size(sumA,2))=-1;
    sumA=[sumA;nuconst];
    sumB=[sumB;-2.1];
    startingvalues=[startingvalues;10];
elseif strcmp(errors,'GED')
    sumA=[sumA';zeros(1,size(sumA,1))]';
    nuconst=zeros(1,size(sumA,2));
    nuconst(size(sumA,2))=-1;
    sumA=[sumA;nuconst];
    sumB=[sumB;-1.1];
    startingvalues=[startingvalues;2];
end

%Estimate the parameters.
stdEstimate =  std(data,1);                      
t=size(data,1);
data  =  [stdEstimate(ones(m,1)) ; data];

warning off;
[parameters, LLF, EXITFLAG, OUTPUT, LAMBDA, GRAD] =  fmincon('multigarch_likelihood', startingvalues ,sumA  , sumB ,[] , [] , LB , UB,[],options,data, p , o, q, garchtype, errortype, stdEstimate);
warning on;
if EXITFLAG<=0
    EXITFLAG
    fprintf(1,'Not Sucessful! \n')
end

parameters(find(parameters(1:p+o+q+1)<  0)) = 0;
parameters(find(parameters(1) <= 0)) = realmin;
[likelihood, ht]=multigarch_likelihood(parameters,data,p,o,q,garchtype,errortype, stdEstimate);
likelihood=-likelihood;

if nargout >= 3
    %Calculate std errors if needed
    hess = hessian_2sided('multigarch_likelihood',parameters,data,p,o,q,garchtype,errortype, stdEstimate);
    stderrors=hess^(-1);
    h=min(abs(parameters/2)+1e-4,max(parameters,1e-2))*eps^(1/3);
    hplus=parameters+h;
    hminus=parameters-h;
    likelihoodsplus=zeros(t,length(parameters));
    likelihoodsminus=zeros(t,length(parameters));
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hplus(i);
        [HOLDER, HOLDER1, indivlike] = multigarch_likelihood(hparameters,data,p,o,q,garchtype,errortype, stdEstimate);
        likelihoodsplus(:,i)=indivlike;
    end
    for i=1:length(parameters)
        hparameters=parameters;
        hparameters(i)=hminus(i);
        [HOLDER, HOLDER1, indivlike] = multigarch_likelihood(hparameters,data,p,o,q,garchtype,errortype, stdEstimate);
        likelihoodsminus(:,i)=indivlike;
    end
    scores=(likelihoodsplus-likelihoodsminus)./(2*repmat(h',t,1));
    scores=scores-repmat(mean(scores),t,1);
    B=scores'*scores;
    robustSE=stderrors*B*stderrors;
end
