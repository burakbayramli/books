function results = ar_g(y,nlag,ndraw,nomit,prior,start)
% PURPOSE: MCMC estimates Bayesian heteroscedastic AR(k) model 
%          imposing stability restrictions using Gibbs sampling
%          y = b0 + y(t-1) b1 + y(t-2) b2 +,...,y(t-k) bk + E, 
%          E = N(0,sige*V), sige = gamma(nu,d0), b = N(c,T), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%---------------------------------------------------
% USAGE:    results = ar_g(y,nlag,ndraw,nomit,prior,start)
% where: y    = dependent variable vector
%        nlag = # of lagged values
%       ndraw = # of draws
%       nomit = # of initial draws omitted for burn-in
%       prior = a structure for prior information input:   
%               prior.beta, prior means for beta,   c above
%               priov.bcov, prior beta covariance , T above
%               prior.rval, r prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on r
%               prior.k,    informative Gamma(m,k) prior on r
%               prior.const, a switch for constant term, 
%                            default = 1 (a constant included)
%               prior.nu,    a prior parameter for sige
%               prior.d0,    a prior parameter for sige
%                            (default = diffuse prior for sige)
%       start = (optional) structure containing starting values: 
%               defaults: OLS beta,sige, V= ones(n,1)
%               start.b   = beta starting values (nvar x 1)
%               start.sig = sige starting value  (scalar)
%               start.V   = V starting values (n x 1)
% ---------------------------------------------------
% RETURNS: a structure:
%          results.meth  = 'ar_g'
%          results.bdraw = bhat draws (ndraw-nomit x nvar)
%          results.sdraw = sige draws (ndraw-nomit x 1)
%          results.vmean = mean of vi draws   (nobs x 1) (if rval input)
%          results.yhat  = mean of posterior y-predicted values
%          results.rdraw = r-value draws (ndraw-nomit x 1)
%          results.pmean = b prior means, prior.beta from input
%          results.pstd  = b prior std deviations sqrt(diag(T))
%          results.r     = value of hyperparameter r (if input)
%          results.nobs  = # of observations
%          results.nadj  = # of observations adjusted for feeding lags
%          results.nvar  = # of variables (including constant term)
%          results.ndraw = # of draws
%          results.nomit = # of initial draws omitted
%          results.y     = actual observations (nobs x 1)
%          results.x     = x-matrix of lagged values of y (nobs-nlag,nlag+const)
%          results.nu    = nu prior parameter
%          results.d0    = d0 prior parameter
%          results.m     = m prior parameter (if input)
%          results.k     = k prior parameter (if input)          
%          results.time  = time taken for sampling
%          results.accept= acceptance rate
%          results.pflag = 'plevel' (default) 
%                          or 'tstat' for bogus t-statistics          
% --------------------------------------------------
% NOTES: a constant term is automatically included in the model
%        unless you set prior.const = 0;
% --------------------------------------------------
% SEE ALSO: prt, prt_gibbs(results), coda
% ----------------------------------------------------
% % REFERENCES: Chib (1993) `Bayes regression with autoregressive
% errors: A Gibbs sampling approach,'  Journal of Econometrics, pp. 275-294.
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[n junk] = size(y);
results.y = y;

% error checking on input
if ~isstruct(prior)
    error('ar_g: must supply the prior as a structure variable');
elseif nargin == 6   % user-supplied starting values
    if ~isstruct(start)
        error('ar_g: must supply starting values in a structure');
    end;
b0 = start.b; sige = start.sig; V = start.V;
sflag = 1;
elseif  nargin == 5  % we supply ols starting values
sflag = 0;
else
error('Wrong # of arguments to ar_g');
end;

fields = fieldnames(prior);
nf = length(fields);
mm = 0; 
rval = 4;  % rval = 4 is default
const = 1; % a constant is the default
nu = 0;    % default diffuse prior for sige
d0 = 0;
for i=1:nf
    if strcmp(fields{i},'rval')
        rval = prior.rval; 
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval
    elseif strcmp(fields{i},'const')
        const = prior.const;
    elseif strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;
    end;
end;

if sflag == 0 % we supply ols starting values
 if const == 1
 x = [ones(n,1) mlag(y,nlag)];
 else
 x = mlag(y,nlag);
 end;
x = trimr(x,nlag,0); % feed the lags
y = trimr(y,nlag,0); 
nadj = length(y);
b0 = x\y;  % Find ols values as initial starting values
k = nlag+const;
sige = (y-x*b0)'*(y-x*b0)/(nadj-k); 
V = ones(nadj,1); in = ones(nadj,1); % initial value for V  
else
 if const == 1
 x = [ones(n,1) mlag(y,nlag)];
 else
 x = mlag(y,nlag);
 end;
x = trimr(x,nlag,0); % feed the lags
y = trimr(y,nlag,0); 
nadj = length(y);
in = ones(nadj,1); % initial value for V  
end;

c = prior.beta;
[checkk,junk] = size(c);
if checkk ~= k
error('ar_g: prior means are wrong');
elseif junk ~= 1
error('ar_g: prior means are wrong');
end;

T = prior.bcov;
[checkk junk] = size(T);
if checkk ~= k
error('ar_g: prior bcov is wrong');
elseif junk ~= k
error('ar_g: prior bcov is wrong');
end;

Q = inv(T);
Qpc = Q*c;


ssave = zeros(ndraw-nomit,1); % storage for draws
bsave = zeros(ndraw-nomit,k);
vmean = zeros(nadj,1);
yhat = zeros(nadj,1);
rsave = zeros(ndraw-nomit,1);

hwait = waitbar(0,'MCMC sampling ...');
t0 = clock;
iter = 1; counter = 0;
while iter <= ndraw; % Start sampling

% generate beta conditional on sige
ys = y.*sqrt(V);
xs = matmul(x,sqrt(V));
xpx = inv(xs'*xs + sige*Q);
beta1 = xpx*(xs'*ys + sige*Qpc);
c = chol(sige*xpx);

accept = 0; % rejection sampling
 while accept == 0;
 beta = beta1 + c'*randn(k,1);
 betap = beta';
 coef = [-fliplr(betap(2:k)) 1];
 root = roots(coef);
 rootmod = abs(root);
 if min(rootmod) >= 1.0001;
  accept = 1;
 else
  counter = counter+1; % counts acceptance rate
  accept = 0;
 end;
end; % end of while loop

% generate sige conditional on beta
nu1 = nadj + nu; 
e = ys - xs*beta;
d1 = d0 + e'*e;
chi = chis_rnd(1,nu1);
t2 = chi/d1;
sige = 1/t2;
  chiv = chis_rnd(nadj,rval+1);   % update vi
  e = y-x*beta;
  vi = ((e.*e./sige) + in*rval)./chiv;
  V = in./vi;  

  if mm ~= 0
  rval = gamm_rnd(1,1,mm,kk);  % update rval
  end;
     
if iter > nomit; % save draws
vmean = vmean + vi;            
ssave(iter-nomit,1) = sige;
bsave(iter-nomit,:) = beta';
yhat = yhat + randn(nadj,1).*sqrt(sige*vi) + x*beta;
    if mm~= 0
        rsave(i-nomit,1) = rval;
    end;
end; % end of if

iter = iter+1;

waitbar(iter/ndraw);
end; % end of while iter < ndraw
gtime = etime(clock,t0);

close(hwait);

vmean = vmean/(ndraw-nomit);
yhat = yhat/(ndraw-nomit);

% find acceptance rate
results.accept = 1 - counter/(iter+counter);
results.meth  = 'ar_g';
results.bdraw = bsave;
results.sdraw = ssave;
results.vmean = vmean;
results.yhat = yhat;
results.pmean = prior.beta;
results.pstd  = sqrt(diag(T));
if mm~= 0
results.rdraw = rsave;
results.m     = mm;
results.k     = kk;
else
results.r     = rval;
results.rdraw = rsave;
end;
results.nobs  = n;
results.nadj  = nadj;
results.nvar  = nlag+const;
results.ndraw = ndraw;
results.nomit = nomit;
results.time = gtime;
results.x = x;
results.nu = nu;
results.d0 = d0;
results.pflag = 'plevel';
