function results = ols_g(y,x,ndraw,nomit,prior,start)
% PURPOSE: MCMC estimates for the Bayesian heteroscedastic linear model
%          y = X B + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          B = N(c,T),  sige = gamma(nu,d0)    
%---------------------------------------------------
% USAGE: results = ols_g(y,x,ndraw,nomit,prior,start)
% where: y    = dependent variable vector
%        x    = independent variables matrix of rank(k)
%       ndraw = # of draws
%       nomit = # of initial draws omitted for burn-in
%       prior = a structure for prior information input
%               prior.beta, prior means for beta,   c above (default diffuse)
%               priov.bcov, prior beta covariance , T above (default diffuse)
%               prior.rval, r prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on r
%               prior.k,    informative Gamma(m,k) prior on r
%               prior.nu,   informative Gamma(nu,d0) prior on sige
%               prior.d0    informative Gamma(nu,d0) prior on sige
%                           default for above: nu=0,d0=0 (diffuse prior)
%       start = (optional) structure containing starting values: 
%               defaults: OLS beta,sige, V= ones(n,1)
%               start.b   = beta starting values (nvar x 1)
%               start.sig = sige starting value  (scalar)
%               start.V   = V starting values (n x 1)
% ---------------------------------------------------
% RETURNS: a structure:
%          results.meth  = 'ols_g'
%          results.bdraw = bhat draws (ndraw-nomit x nvar)
%          results.vmean = mean of vi draws (nobs x 1)
%          results.sdraw = sige draws (ndraw-nomit x 1)
%          results.yhat  = mean of draws from posterior for y-predicted
%          results.rdraw = r-value draws (ndraw-nomit x 1), if Gamma(m,k) prior 
%          results.pmean = b prior means (prior.beta from input)
%          results.pstd  = b prior std deviation, sqrt(prior.bcov)
%          results.m     = prior m-value for r hyperparameter (if input)
%          results.k     = prior k-value for r hyperparameter (if input)
%          results.r     = value of hyperparameter r (if input)
%          results.nu    = prior nu-value for sige prior
%          results.d0    = prior d0-value for sige prior
%          results.nobs  = # of observations
%          results.nvar  = # of variables
%          results.ndraw = # of draws
%          results.nomit = # of initial draws omitted
%          results.y     = actual observations
%          results.x     = x-matrix
%          results.time  = time taken for sampling
%          results.pflag = 'plevel' (default) 
%                          or 'tstat' for bogus t-statistics
% --------------------------------------------------
% NOTE: use either improper prior.rval 
%       or informative Gamma prior.m, prior.k, not both of them
%---------------------------------------------------
% SEE ALSO: coda, gmoment, prt_gibbs(results)
%---------------------------------------------------
% REFERENCES: Geweke (1993)  'Bayesian Treatment of the 
% Independent Student-$t$ Linear Model', Journal of Applied
% Econometrics, 8, s19-s40.
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[n k] = size(x);   

if nargin > 4
% error checking on input
if ~isstruct(prior)
    error('ols_g: must supply the prior as a structure variable');
end;
end;

if nargin == 6   % user-supplied starting values
    if ~isstruct(start)
        error('ols_g: must supply starting values in a structure');
    end;
b0 = start.b; sige = start.sig; V = start.V;
end;

if  nargin == 5  % ols starting values
b0 = (x'*x)\(x'*y);  % Find ols values as initial starting values
sige = (y-x*b0)'*(y-x*b0)/(n-k); 
V = ones(n,1); in = ones(n,1); % initial value for V  
end;

if nargin == 4 % default values
b0 = (x'*x)\(x'*y);  % Find ols values as initial starting values
sige = (y-x*b0)'*(y-x*b0)/(n-k); 
V = ones(n,1); in = ones(n,1); % initial value for V 
mm = 0; rval = 4; % rval = 4 is default
nu = 0; d0 = 0; % default to a diffuse prior on sige
T = eye(k)*1e+12; c = zeros(k,1);
end;

if nargin < 4
error('Wrong # of arguments to ols_g');
end;

if nargin > 4
fields = fieldnames(prior);
nf = length(fields);
mm = 0; rval = 4; % rval = 4 is default
nu = 0; d0 = 0; % default to a diffuse prior on sige
c = zeros(k,1); T = eye(k)*1e+12;
 for i=1:nf
    if strcmp(fields{i},'rval')
        rval = prior.rval; 
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval
    elseif strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;   
    elseif strcmp(fields{i},'beta');
    c = prior.beta;
    elseif strcmp(fields{i},'bcov');
    T = prior.bcov;
    end;
 end;
end;

[checkk,junk] = size(c);
if checkk ~= k
error('ols_g: prior means are wrong');
elseif junk ~= 1
error('ols_g: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('ols_g: prior bcov is wrong');
elseif junk ~= k
error('ols_g: prior bcov is wrong');
end;


Q = inv(T); Qpc = Q*c;

bsave = zeros(ndraw-nomit,k);    % allocate storage for results
ssave = zeros(ndraw-nomit,1); 
rsave = zeros(ndraw-nomit,1);
vmean = zeros(n,1);
yhat = zeros(n,1);

hwait = waitbar(0,'MCMC sampling ...');
t0 = clock;
for i=1:ndraw; % Start the sampling 
          ystar = matmul(sqrt(V),y); 
          xstar = matmul(x,sqrt(V));
          xpxi = inv(xstar'*xstar + sige*Q); 
          xpy = (xstar'*ystar + sige*Qpc); 
          % update b  
          b = xpxi*xpy;    
       a = chol(xpxi);
       b = sqrt(sige)*a'*randn(k,1) + b;          

         % update sige 
          nu1 = n + nu; 
          e = ystar - xstar*b;
          d1 = d0 + e'*e;
          chi = chis_rnd(1,nu1);
          t2 = chi/d1;
          sige = 1/t2;
          
          % update vi
          e = y - x*b;
          chiv = chis_rnd(n,rval+1);   
          vi = ((e.*e./sige) + in*rval)./chiv;
          V = in./vi;   
  
         % update rval
         if mm ~= 0           
         rval = gamm_rnd(1,1,mm,kk);  
         end;
    if i > nomit % if we are past burn-in, save the draws
    bsave(i-nomit,:) = b';
    ssave(i-nomit,1) = sige;
    yhat = yhat + randn(n,1).*sqrt(sige*vi) + x*b;

    vmean = vmean + vi;
    if mm~= 0
        rsave(i-nomit,1) = rval;
    end;
end;
waitbar(i/ndraw);
end;          % End the sampling
gtime = etime(clock,t0);
close(hwait);

vmean = vmean/(ndraw-nomit);
yhat = yhat/(ndraw-nomit);

% return results
results.meth  = 'ols_g';
results.bdraw = bsave;
results.pmean = c;
results.pstd  = sqrt(diag(T));
results.vmean = vmean;
results.sdraw = ssave;
results.yhat = yhat;
if mm~= 0
results.rdraw = rsave;
results.m     = mm;
results.k     = kk;
else
results.r     = rval;
results.rdraw = rsave;
end;
results.nobs  = n;
results.nvar  = k;
results.y     = y;
results.x     = x;
results.nu    = nu;
results.d0    = d0;
results.time = gtime;
results.ndraw = ndraw;
results.nomit = nomit;
results.pflag = 'plevel';
