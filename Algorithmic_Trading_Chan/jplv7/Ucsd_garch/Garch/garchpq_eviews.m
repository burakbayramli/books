function [parameters, likelihood, ht, stderrors, robustSE, scores, grad] = garchpq(data , p , q , startingvals, options)
% PURPOSE:
%     GARCH(P,Q) parameter estimation with normal innovations using Eviews type constraints
% 
% USAGE:
%     [parameters, likelihood, ht, stderrors, robustSE, scores, grad] = garchpq(data , p , q , startingvals, options)
% 
% INPUTS:
%     data: A single column of zero mean random data, normal or not for quasi likelihood
%  
%     P: Non-negative, scalar integer representing a model order of the ARCH 
%       process
% 
%     Q: Positive, scalar integer representing a model order of the GARCH 
%       process: Q is the number of lags of the lagged conditional variances included
%       Can be empty([]) for ARCH process
% 
%     startingvals: A (1+p+q) vector of starting vals.  If you do not provide, a naieve guess of 1/(2*max(p,q)+1) is
%       used for the arch and garch parameters, and omega is set to make the real unconditional variance equal
%       to the garch expectation of the expectation.
% 
%     options: default options are below.  You can provide an options vector.  See HELP OPTIMSET
% 
% OUTPUTS:
%     parameters : a [1+p+q X 1] column of parameters with omega, alpha1, alpha2, ..., alpha(p)
%                 beta1, beta2, ... beta(q)
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
%     grad = the average score at the parameters
% 
% COMMENTS:
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
%  uses GARCH_LIKELIHOOD and GARCHCORE.  You should MEX, mex 'path\garchcore.c', the MEX source 
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


if nargin<=3 | isempty(startingvals)
    guess  =  1/(2*m+1);
    alpha  =  .15*ones(p,1)/p;
    beta   =  .75*ones(q,1)/q;
    omega  =  (1-(sum(alpha)+sum(beta)))*cov(data);  %set the uncond = to its expection
else
    omega=startingvals(1);
    alpha=startingvals(2:p+1);
    beta=startingvals(p+2:p+q+1);
end


LB         =  [];     
UB         =  [];     
sumA =  [-eye(1+p) zeros(1+p,q)];
sumB =  [zeros(1+p,1)];                          


if (nargin <= 4) | isempty(options)
    options  =  optimset('fmincon');
    options  =  optimset(options , 'TolFun'      , 1e-003);
    options  =  optimset(options , 'Display'     , 'iter');
    options  =  optimset(options , 'Diagnostics' , 'on');
    options  =  optimset(options , 'LargeScale'  , 'off');
    options  =  optimset(options , 'MaxFunEvals' , 400*(1+p+q));
    options  =  optimset(options , 'GradObj'     , 'off');
end
sumB = sumB - [zeros(1+p,1)]*2*optimget(options, 'TolCon', 1e-6);


stdEstimate =  std(data,1);                      
data        =  [stdEstimate(ones(m,1)) ; data];
% Estimate the parameters.

warning off;
[parameters, LLF, EXITFLAG, OUTPUT, LAMBDA, GRAD] =  fmincon('garcheviewslikelihood', [omega ; alpha ; beta] ,sumA  , sumB ,[] , [] , LB , UB,'garcheviewscon',options,data, p , q, m, stdEstimate);
warning on;

if EXITFLAG<=0
    EXITFLAG
    fprintf(1,'Not Sucessful! \n')
end
