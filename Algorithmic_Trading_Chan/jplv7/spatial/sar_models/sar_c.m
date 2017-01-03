function results = sar_c(y,x,W,prior)
% PURPOSE: Bayesian log-marginal posterior for the spatial autoregressive model
%          y = rho*W*y + XB + e, e = N(0,sige*In)
%          B = N[0,inv(g*X'X)], Zellner's g-prior
%          1/sige = Gamma(nu,d0)
%          rho = beta(a1,a2)
%-------------------------------------------------------------
% USAGE: results = sar_c(y,x,W,prior)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix, WITH INTERCEPT TERM AS 1ST column
%            (should be well-scaled or studentized to accomodate the g-prior mean of zero)
%        W = spatial weight matrix (standardized, row-sums = 1)
%    prior = a structure variable with:
%            prior.g     = g prior parameter, default 1/max(n,k*k)
%            prior.nu    = informative Gamma(nu,d0) prior on sige
%            prior.d0    = default: nu=0,d0=0 (diffuse prior)
%            prior.a1    = parameter for beta(a1,a2) prior on rho (default = 1.01)
%            prior.a2    = (default = 1.01) see: 'help beta_prior'
%            prior.rmin  = (optional) min rho used in sampling (default = -1)
%            prior.rmax  = (optional) max rho used in sampling (default = 1)  
%            prior.lflag = 0 for full lndet computation (default = 1, fastest)
%                        = 1 for MC approx (fast for large problems)
%                        = 2 for Spline approx (medium speed)
%            prior.order = order to use with prior.lflag = 1 option (default = 50)
%            prior.iter  = iters to use with prior.lflag = 1 option (default = 30) 
%            prior.lndet = a matrix returned by sar, sar_g, sarp_g, etc.
%                          containing log-determinant information to save time
%-------------------------------------------------------------
% RETURNS:  a structure:
%          results.meth   = 'sar_c'
%          results.nobs   = # of observations
%          results.nvar   = # of variables in x-matrix
%          results.y      = y-vector from input (nobs x 1)
%          results.nu     = nu prior parameter
%          results.d0     = d0 prior parameter
%          results.a1     = a1 parameter for beta prior on rho from input, or default value
%          results.a2     = a2 parameter for beta prior on rho from input, or default value
%          results.time1  = time for log determinant calcluation
%          results.time2  = time for log-marginal posterior calculation
%          results.time   = total time taken  
%          results.rmax   = rmax from input
%          results.rmin   = rmin from input         
%          results.lflag  = lflag from input
%          results.iter   = prior.iter option from input
%          results.order  = prior.order option from input
%          results.limit  = matrix of [rho lower95,logdet approx, upper95] 
%                           intervals for the case of lflag = 1
%          results.lndet = a matrix containing log-determinant information
%                          (for use in later function calls to save time)
%          results.mlike = log marginal likelihood (a vector ranging over
%                          rmin < rho < rmax values that can be integrated for model comparison)
% --------------------------------------------------------------
% NOTES: - returns only the log-marginal posterior for model comparison purposes
%          NO ESTIMATES returned
% - use a1 = 1.0 and a2 = 1.0 for uniform prior on rho
% - results.mlike can be used for model comparison 
%   using model_probs() function (see model_compare.m for a demonstration)
% --------------------------------------------------------------
% SEE ALSO: sar_g, for estimation of the model parameters
% --------------------------------------------------------------
% REFERENCES: James P. LeSage, `Bayesian Estimation of Spatial Autoregressive
%             Models',  International Regional Science Review, 1997 
%             Volume 20, number 1\&2, pp. 113-129.
% For lndet information see: Ronald Barry and R. Kelley Pace, 
% "A Monte Carlo Estimator of the Log Determinant of Large Sparse Matrices", 
% Linear Algebra and its Applications", Volume 289, Number 1-3, 1999, pp. 41-54.
% and: R. Kelley Pace and Ronald P. Barry 
% "Simulating Mixed Regressive Spatially autoregressive Estimators", 
% Computational Statistics, 1998, Vol. 13, pp. 397-418.
%----------------------------------------------------------------

% written by:
% James P. LeSage, last updated 6/2004
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% NOTE: some of the speed for large problems comes from:
% the use of methods pioneered by Pace and Barry.
% R. Kelley Pace was kind enough to provide functions
% lndetmc, and lndetint from his spatial statistics toolbox
% for which I'm very grateful.

timet = clock;

% error checking on inputs
[n junk] = size(y);
[n1 k] = size(x);
[n3 n4] = size(W);
time1 = 0;
time2 = 0;

% see if the user input a vector of ones
tst = sum(x(:,1));
if (tst ~= n)
    error('sar_c: you must have intercept term in the 1st column of the x-matrix');
end;

nobsa = n;

results.nobs  = n;
results.nvar  = k;
results.y = y;      

if n1 ~= n
error('sar_c: x-matrix contains wrong # of observations');
elseif n3 ~= n4
error('sar_c: W matrix is not square');
elseif n3~= n
error('sar_c: W matrix is not the same size at y,x');
end;

if nargin == 3
    prior.lflag = 1;
end;

[nu,d0,rho,sige,rmin,rmax,detval,ldetflag,order,iter,a1,a2,g] = sar_parse(prior,x);

results.order = order;
results.iter = iter;

timet = clock; % start the timer

[detval,time1] = sar_lndet(ldetflag,W,rmin,rmax,detval,order,iter);

timelog = clock;

[nobs,k] = size(x);
xt = x;
Wy = sparse(W)*y;
iota = ones(nobs,1);
Wy = sparse(W)*y;

% now compute vectorized log-marginal likelihood
% to use in trapezoid numerical integration over rho
          xpx = xt'*xt;
          b0t = (xpx)\(xt'*y);
          bdt = (xpx)\(xt'*Wy);
          e0 = y - xt*b0t;
          ed = Wy - xt*bdt;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
          my = mean(y);
          mWy = mean(Wy);
          
          Qvec = zeros(length(detval(:,1)),1);
          for i=1:length(detval(:,1));
              ybar = my - detval(i,1)*mWy;
              yhat = y - detval(i,1)*Wy - ybar*iota;
              Qvec(i,1) = (g/(1+g))*(yhat'*yhat);
          end;

% evaluate vectorized log-marginal likelihood
lmout = sar_marginal(detval,epe0,eped,epe0d,nobs,k,a1,a2,g,Qvec);

results.mlike = lmout;
results.lndet = detval;

time2 = etime(clock,timelog);

time = etime(clock,timet);

results.time1 = time1;
results.time2 = time2;
results.time = time;


function [nu,d0,rho,sige,rmin,rmax,detval,ldetflag,order,iter,a1,a2,g] = sar_parse(prior,x)
% PURPOSE: parses input arguments for sar_c models
% ---------------------------------------------------
%  USAGE: [nu,d0,rho,sige,rmin,rmax,detval, ...
%         ldetflag,order,iter,a1,a2,g] = sar_parse(prior,k)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% set defaults
[nobs,k] = size(x);
g1 = (k*k); % Fernandez, Ley, Steel defaults
g2 = nobs;
g = 1/max([g1 g2]);

ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
order = 50;    % there are parameters used by the MC det approx
iter = 30;     % defaults based on Pace and Barry recommendation
rmin = -1;     % use -1,1 rho interval as default
rmax = 1;
detval = 0;    % just a flag
rho = 0.5;
sige = 1.0;
nu = 0;
d0 = 0;
a1 = 1.01;
a2 = 1.01;

fields = fieldnames(prior);
nf = length(fields);
if nf > 0
 for i=1:nf
    if strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;  
    elseif strcmp(fields{i},'a1')
       a1 = prior.a1; 
    elseif strcmp(fields{i},'a2')
       a2 = prior.a2; 
    elseif strcmp(fields{i},'g')
       g = prior.g; 
    elseif strcmp(fields{i},'rmin')
        rmin = prior.rmin; eflag = 0;
    elseif strcmp(fields{i},'rmax')
        rmax = prior.rmax;  eflag = 0;
    elseif strcmp(fields{i},'lndet')
    detval = prior.lndet;
    ldetflag = -1;
    eflag = 0;
    rmin = detval(1,1);
    nr = length(detval);
    rmax = detval(nr,1);
    elseif strcmp(fields{i},'lflag')
        tst = prior.lflag;
        if tst == 0,
        ldetflag = 0; 
        elseif tst == 1,
        ldetflag = 1; 
        elseif tst == 2,
        ldetflag = 2; 
        else
        error('sar_c: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = prior.order;  
    elseif strcmp(fields{i},'iter')
        iter = prior.iter; 
    end;
 end;

 
else, % the user has input a blank info structure
      % so we use the defaults
end; 


function [detval,time1] = sar_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
% PURPOSE: compute the log determinant |I_n - rho*W|
% using the user-selected (or default) method
% ---------------------------------------------------
%  USAGE: detval = far_lndet(lflag,W,rmin,rmax)
% where eflag,rmin,rmax,W contains input flags 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------


% do lndet approximation calculations if needed
if ldetflag == 0 % no approximation
t0 = clock;    
out = lndetfull(W,rmin,rmax);
time1 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];
    
elseif ldetflag == 1 % use Pace and Barry, 1999 MC approximation

t0 = clock;    
out = lndetmc(order,iter,W,rmin,rmax);
time1 = etime(clock,t0);
results.limit = [out.rho out.lo95 out.lndet out.up95];
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];

elseif ldetflag == 2 % use Pace and Barry, 1998 spline interpolation

t0 = clock;
out = lndetint(W,rmin,rmax);
time1 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];

elseif ldetflag == -1 % the user fed down a detval matrix
    time1 = 0;
        % check to see if this is right
        if detval == 0
            error('sar_c: wrong lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('sar_c: wrong sized lndet input argument');
        elseif n1 == 1
            error('sar_c: wrong sized lndet input argument');
        end;          
end;

function  out = sar_marginal(detval,epe0,eped,epe0d,nobs,nvar,a1,a2,g,Qvec)
% PURPOSE: returns a vector of the log-marginal over a grid of rho-values
%          for the case of Zellner g-prior on beta
% -------------------------------------------------------------------------
% USAGE: out = sar_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,a1,a2,g,Qvec)
% where:       detval = an ngrid x 2 matrix with rho-values and lndet values
%                  e0 = y - x*b0;
%                 ed = Wy - x*bd;
%               epe0 = e0'*e0;
%               eped = ed'*ed;
%              epe0d = ed'*e0;
%               nobs = # of observations
%               nvar = # of explanatory variables
%                 a1 = parameter for beta prior on rho
%                 a2 = parameter for beta prior on rho
%                 g = g-prior value
%              Qvec = vectorized (g/(1+g))*(yhat'*yhat), over all rho-values
%                     ybar = mean(y) - detval(i,1)*mean(Wy);
%                     yhat = y - detval(i,1)*Wy - ybar*iota;
%                     
% -------------------------------------------------------------------------
% RETURNS: out = a structure variable
%        out = log marginal, a vector the length of detval
% -------------------------------------------------------------------------

% written by:
% James P. LeSage, 6/2004
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

n = length(detval);
no2 = nobs/2;
nm1o2 = (nobs-1)/2;
bprior = beta_prior(detval(:,1),a1,a2);
iota = ones(n,1);
z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
logdetx = (nvar/2)*log(g/(1+g));
kterm = gammaln(nm1o2) -no2*log(pi);
gterm = 1/(1+g);
%den = logdetx*iota + log(bprior) + detval(:,2) - nm1o2*log(gterm*z + Qvec);
den = logdetx*iota + log(bprior) + detval(:,2) - nm1o2*log(z);

out = kterm + den;


function out = beta_prior(rvec,a1,a2)
% PURPOSE: construct beta-prior for rho over -1,1 interval
%-----------------------------------------------------------
% USAGE: out = beta_prior(a1,a2,rvec);
% where:    rvec = grid over rmin,rmax interval, an n x 1 vector
%           a1 = (optional) prior parameter (default = 1.1)
%           a2 = (optional) prior parameter (default = 1.1)
% RETURNS: out = nx1 vector of prior weights for rho
%          in the interval rmin,rmax
%-----------------------------------------------------------
% NOTES: increasing a1,a2 to 1.5,1.5 or 2.0,2.0 increases
%        prior weight placed on zero for rho, and decreases
%        weights on non-zero values for rho
% to see what the prior looks like:
% rvec = -1:0.01:1;
% a1 = 1.1; a2 = 1.1;
% bprior = beta_prior(rvec',a1,a2);
% plot(rvec,bprior);
%-----------------------------------------------------------

% written by:
% James P. LeSage, 4/2003
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if nargin == 1
a1 = 1.01;
a2 = 1.01;
elseif nargin == 2
    a2 = 1.01;
elseif nargin > 3
    error('beta_prior: wrong # of inputs');
end;

B = beta(a1,a2);
num = (1+rvec).^(a1-1);
num = num.*(1-rvec).^(a2-1);
den = 2^(a1+a2-1);
out = (1/B)*num/den;
out(1) = realmin;
out(end) = realmin;

