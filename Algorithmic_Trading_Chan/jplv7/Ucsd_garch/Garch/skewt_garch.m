function [parameters, likelihood, stderrors, robustSE, ht, scores] = skewt_garch(data , p , q , startingvals, options)
% PURPOSE:
%     SKEWT_GARCH(P,Q) parameter estimation with different error distributions, the NOrmal, The T, 
%     and the Generalized Error Distribution
% 
% USAGE:
%     [parameters, likelihood, stderrors, robustSE, ht, scores] = skewt_garch(data , p , q , startingvals, options, type)
% 
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
%     startingvals: A (1+p+q) (plus 1 if STUDENTT OR GED is selected for the nu parameter) vector of starting vals.
%       If you do not provide, a naieve guess of 1/(2*max(p,q)+1) is used for the arch and garch parameters,
%       and omega is set to make the real unconditional variance equal
%       to the garch expectation of the expectation.
% 
%      options: default options are below.  You can provide an options vector.  See HELP OPTIMSET
% 
% OUTPUTS:
%     parameters : a [1+p+q X 1] column of parameters with omega, alpha1, alpha2, ..., alpha(p)
%                 beta1, beta2, ... beta(q), nu, lambda
% 
%     likelihood = the loglikelihood evaluated at he parameters
% 
%     robustSE = QuasiLikelihood std errors which are robust to some forms of misspecification(see White 94)
% 
%     stderrors = the inverse analytical hessian, not for quasi maximum liklihood
% 
%     ht = the estimated time varying VARIANCES
% 
%     scores = The numberical scores(# of params by t) for M testing   
% 
% COMMENTS:
%   SKEWT_GARCH(P,Q) the following(wrong) constratins are used(they are right for the (1,1) case or any Arch case
%     (1) Omega > 0
%     (2) Alpha(i) >= 0 for i = 1,2,...P
%     (3) Beta(i)  >= 0 for i = 1,2,...Q
%     (4) sum(Alpha(i) + Beta(j)) < 1 for i = 1,2,...P and j = 1,2,...Q
%     (5) nu>2 of Students T and nu>1 for GED
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
%
%
%  uses SKEWT_GARCHLIKELIHOOD and GARCHCORE.  You should MEX, mex 'path\garchcore.c', the MEX source 
%  The included MEX is for R12 Windows and was compiled with VC++6. It gives a 10-15 times speed improvement
%
% Author: Andrew Patton
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

t=size(data,1);

if nargin<5
    options=[];
end

if size(data,2) > 1
   error('Data series must be a column vector.')
elseif isempty(data)
   error('Data Series is Empty.')
end

if (length(q) > 1) | any(q < 0)
   error('Q must ba a single positive scalar or 0 for ARCH.')
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


if nargin<=3 | isempty(startingvals)
   guess    = 1/(2*m+1);
   alpha    =  .1*ones(p,1)/p;
   beta     =  .82*ones(q,1)/q;
   omega    = (1-(sum(alpha)+sum(beta)))*cov(data);  %set the uncond = to its expection
   nu       = 30;
   lambda   = 0;
else
   omega=startingvals(1);
   alpha=startingvals(2:p+1);
   beta=startingvals(p+2:p+q+1);
   nu  = startingvals(p+q+2);
   lambda = startingvals(p+q+3);
end



sumA =  [-eye(1+p+q) zeros(1+p+q,2);...
      0  ones(1,p)  ones(1,q) zeros(1,2);...
      zeros(1,1+p+q) -1 0];

sumB =  [zeros(1+p+q,1); 1 ; -4.1];                          
LB         =  [zeros(1,1+p+q) 4.1 -1];     
UB         =  [ones(1,1+p+q) inf 1];     

if (nargin <= 5) | isempty(options)
   options  =  optimset('fmincon');
   options  =  optimset(options , 'TolFun'      , 1e-006);
   options  =  optimset(options , 'Display'     , 'iter');
   options  =  optimset(options , 'Diagnostics' , 'on');
   options  =  optimset(options , 'LargeScale'  , 'off');
   options  =  optimset(options , 'MaxFunEvals' , 400*(2+p+q));
end

sumB = sumB - [zeros(1+p+q,1); 1; 0]*2*optimget(options, 'TolCon', 1e-6);
startingvals = [omega ; alpha ; beta; nu; lambda];
stdEstimate =  std(data,1);                     
m=max(p,q);
data=[stdEstimate(ones(m,1)) ; data]; 

warning off;

[parameters, LLF, EXITFLAG, OUTPUT, LAMBDA, GRAD] =  fmincon('skewt_garchlikelihood', startingvals ,sumA  , sumB ,[] , [] , LB , UB,[],options, data, p , q, m,stdEstimate);

if EXITFLAG<=0
   EXITFLAG
   fprintf(1,'Not Sucessful! \n')
end

if nargout>1
parameters(find(parameters(1:1+p+q) <  0)) = 0;          
parameters(find(parameters(1) <= 0)) = realmin;    
hess = hessian_2sided('skewt_garchlikelihood',parameters,data,p , q, m,stdEstimate);
[likelihood, ht]=skewt_garchlikelihood(parameters,data,p , q, m,stdEstimate);
likelihood=-likelihood;
stderrors=hess^(-1);
end

if nargout > 4
   h=max(abs(parameters/2),1e-2)*eps^(1/3);
   hplus=parameters+h;
   hminus=parameters-h;
   likelihoodsplus=zeros(t,length(parameters));
   likelihoodsminus=zeros(t,length(parameters));
   for i=1:length(parameters)
      hparameters=parameters;
      hparameters(i)=hplus(i);
      [HOLDER, HOLDER1, indivlike] = skewt_garchlikelihood(hparameters,data,p , q,m, stdEstimate);
      likelihoodsplus(:,i)=indivlike;
   end
   for i=1:length(parameters)
      hparameters=parameters;
      hparameters(i)=hminus(i);
      [HOLDER, HOLDER1, indivlike] = skewt_garchlikelihood(hparameters,data,p , q, m,stdEstimate);
      likelihoodsminus(:,i)=indivlike;
   end
   scores=(likelihoodsplus-likelihoodsminus)./(2*repmat(h',t,1));
   scores=scores-repmat(mean(scores),t,1);
   B=scores'*scores;
   robustSE=stderrors*B*stderrors;
end
