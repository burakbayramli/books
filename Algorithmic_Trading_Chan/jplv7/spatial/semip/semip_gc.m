function results = semip_gc(y,x,W,m,mobs,ndraw,nomit,prior)
% PURPOSE: C-MEX version of: Bayesian Probit with spatial individual effects:
%         Y = (Yi, i=1,..,m) with each vector, Yi = (yij:j=1..Ni) consisting of individual 
%          dichotomous observations in regions i=1..m, defined by yij = Indicator(zij>0), 
%          where latent vector Z = (zij) is given by the linear model:
%
%          Z = X*b + del*a + e   with:
%
%          x = n x k matrix of explanatory variables [n = sum(Ni: i=1..m)]; 
%             del = n x m indicator matrix with del(j,i) = 1 iff indiv j is in reg i;
%          a = (ai: i=1..m) a vector of random regional effects modeled by
%
%          a = rho*W*a + U,     U ~ N[0,sige*I_m] ; (I_m = m-square Identity matrix)
%
%          and with e ~ N(0,V), V = diag(del*v) where v = (vi:i=1..m). 
%
%          The priors for the above parameters are of the form:
%          r/vi ~ ID chi(r), r ~ Gamma(mm,kk)
%          b ~ N(c,T),  
%          1/sige ~ Gamma(nu,d0), 
%          rho ~ Uniform(1/lmin,1/lmax) 
%-----------------------------------------------------------------
% USAGE: results = semip_gc(y,x,W,m,mobs,ndraw,nomit,prior)
% where: y = dependent variable vector (nobs x 1) [must be zero-one]
%        x = independent variables matrix (nobs x nvar)
%        W = 1st order contiguity matrix (standardized, row-sums = 1)
%        m = # of regions (= m above)
%     mobs = an m x 1 vector containing the # of observations in each
%            region [= (Ni:i=1..m) above]
%    ndraw = # of draws
%    nomit = # of initial draws omitted for burn-in            
%    prior = a structure variable with:
%            prior.beta  = prior means for beta,  (= c above) 
%                          (default = 0)
%            prior.bcov  = prior beta covariance , (= T above)  
%                          [default = 1e+12*I_k ]
%            prior.rval  = r prior hyperparameter, default=4
%            prior.mm     = informative Gamma(mm,kk) prior on r
%            prior.kk     = (default: not used)
%            prior.nu    = informative Gamma(nu,d0) prior on sige
%            prior.d0    = default: nu=0,d0=0 (diffuse prior)
%            prior.rmin  = (optional) min rho used in sampling (default = 0)
%            prior.rmax  = (optional) max rho used in sampling (default = 1)  
%            prior.lflag = 0 for full lndet computation (default = 1, fastest)
%                        = 1 for MC approx (fast for large problems)
%                        = 2 for Spline approx (medium speed)
%            prior.eflag = 0 for no eigenvalue calculations, 
%                        = 1 for eigenvalue bounds on rho
%            prior.order = order to use with prior.lflag = 1 option (default = 50)
%            prior.iter  = iters to use with prior.lflag = 1 option (default = 30) 
%            prior.seed  = a numerical value to seed the random number generator
%                         (default is to use the system clock which produces
%                          different results on every run)
%---------------------------------------------------
% RETURNS:  a structure:
%          results.meth   = 'semip_gc'
%          results.bdraw  = bhat draws (ndraw-nomit x nvar)
%          results.pdraw  = rho  draws (ndraw-nomit x 1)
%          results.adraw  = a draws (ndraw-nomit x m)
%          results.amean  = mean of a draws (m x 1)
%          results.sdraw  = sige draws (ndraw-nomit x 1)
%          results.vmean  = mean of vi draws (m x 1) 
%          results.rdraw  = r draws (ndraw-nomit x 1) (if m,k input)
%          results.bmean  = b prior means, prior.beta from input
%          results.bstd   = b prior std deviations sqrt(diag(prior.bcov))
%          results.bflag  = 1 for informative prior, 0 for diffuse
%          results.r      = value of hyperparameter r (if input)
%          results.rsqr   = R-squared
%          results.nobs   = # of observations
%          results.mobs   = mobs vector from input
%          results.m      = # of regions
%          results.nvar   = # of variables in x-matrix
%          results.ndraw  = # of draws
%          results.nomit  = # of initial draws omitted
%          results.y      = actual observations (nobs x 1)
%          results.zmean  = mean of latent z-draws (nobs x 1)
%          results.yhat   = mean of posterior y-predicted (nobs x 1)
%          results.nu     = nu prior parameter
%          results.d0     = d0 prior parameter
%          results.time1  = time for eigenvalue calculation
%          results.time2  = time for log determinant calculation
%          results.time3  = time for sampling
%          results.time   = total time taken 
%          results.rmax   = 1/max eigenvalue of W (or rmax if input)
%          results.rmin   = 1/min eigenvalue of W (or rmin if input)       
%          results.eflag  = 0 for no eigenvalue calculations, 1 for eigenvalues   
%          results.tflag  = 'plevel' (default) for printing p-levels
%                         = 'tstat' for printing bogus t-statistics 
%          results.prho   = prior for rho (from input)
%          results.rvar   = prior variance for rho (from input)
%          results.pflag  = flag for normal prior on rho, =0 diffuse, =1 normal prior
%          results.lflag  = lflag from input
%          results.iter   = prior.iter  option from input
%          results.order  = prior.order option from input
%          results.limit  = matrix of [rho lower95,logdet approx, upper95] 
%                           intervals for the case of lflag = 1 
%          results.seed   = seed (if input, or zero if not)
% ----------------------------------------------------
% NOTES: see c_source/semip folder for source code semip_gcc.c
%        compile with: mex semip_gcc.c matrixjpl.c randomlib.c   
% If you input a continuous y-vector, no truncated draws are done
% producing spatial regression estimates with individual effects in place
% of spatial probit estimates.
% ----------------------------------------------------
% SEE ALSO: semip_gd, prt, semip_g.m, a matlab function in lieu of this C-MEX function
% ----------------------------------------------------
% REFERENCES: Tony E. Smith and James LeSage
% "A Bayesian Probit Model with Spatial Dependencies" unpublished manuscript
%
% For lndet information see: Ronald Barry and R. Kelley Pace, "A Monte Carlo Estimator
% of the Log Determinant of Large Sparse Matrices", Linear Algebra and
% its Applications", Volume 289, Number 1-3, 1999, pp. 41-54.
% and: R. Kelley Pace and Ronald P. Barry "Simulating Mixed Regressive
% Spatially autoregressive Estimators", Computational Statistics, 1998,
% Vol. 13, pp. 397-418.
%----------------------------------------------------------------

% written by:                                        and:
% James P. LeSage, Dept of Economics            Tony E. Smith
% University of Toledo                              Dept of Systems Engineering
% 2801 W. Bancroft St,                              University of Pennsylvania
% Toledo, OH 43606                                  Philadelphia, PA 19104
% jpl@jpl.econ.utoledo.edu                          tesmith@ssc.upenn.edu

% NOTE: much of the speed for large problems comes from:
% the use of methods pioneered by Pace and Barry.
% R. Kelley Pace was kind enough to provide functions
% lndetmc, and lndetint from his spatial statistics toolbox
% for which we are very grateful.

timet = clock;

time1 = 0;
time2 = 0;
time3 = 0;

% error checking on inputs
[n junk] = size(y);
results.y = y;
[n1 k] = size(x);
[n3 n4] = size(W);

if n1 ~= n
error('semip_gc: x-matrix contains wrong # of observations');
elseif n3 ~= n4
error('semip_gc: W matrix is not square');
elseif n3~= m
error('semip_gc: W matrix is not the same size as # of regions');
end;

% check that mobs vector is correct
obs_chk = sum(mobs);
if obs_chk ~= n
error('semip_gc: wrong # of observations in mobs vector');
end;
if length(mobs) ~= m
error('semip_gc: wrong size mobs vector -- should be m x 1');
end;

% set defaults
seed = 0;
seedflag = 0;
mm = 0; % default for mm
kk = 0;  % default for kk (not used if mm = 0)
rval = 4;  % default for r
nu = 0;    % default diffuse prior for sige
d0 = 0;
sig0 = 1;  % default starting values for sige
c = zeros(k,1);   % diffuse prior for beta
T = eye(k)*1e+12;
bflag = 0;
p0 = 0.5;         % default starting value for rho
inV0 = ones(m,1);   % default starting value for inV [= inv(V)]
a0 = ones(m,1);
lflag = 1; % use Pace's fast MC approximation to lndet(I-rho*W)
eflag = 0;
rmin = 01;  % use -1,1 rho interval
rmax = 1;
order = 50; iter = 30; % defaults
pflag = 0; % flag for prior on rho

if nargin == 8   % parse input values
 fields = fieldnames(prior);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'rval')
        rval = prior.rval; 
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval
    elseif strcmp(fields{i},'beta')
        c = prior.beta; bflag = 1;
    elseif strcmp(fields{i},'bcov')
        T = prior.bcov; bflag = 1;
    elseif strcmp(fields{i},'prho')
        prho = prior.prho; pflag = 1;
    elseif strcmp(fields{i},'pvar')
        pvar = prior.pvar; pflag = 1;
    elseif strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;
    elseif strcmp(fields{i},'rmin')
    rmin = prior.rmin;
    rmax = prior.rmax;
    elseif strcmp(fields{i},'eflag')
    eflag = prior.eflag;
    elseif strcmp(fields{i},'lflag')
        lflag = prior.lflag;
    elseif strcmp(fields{i},'order')
        order = prior.order;  results.order = order;
    elseif strcmp(fields{i},'iter')
       iter = prior.iter; results.iter = iter;
    elseif strcmp(fields{i},'seed');
    seed = prior.seed;
    seedflag = 1;    
    end;
 end;

elseif nargin == 7   % we supply all defaults

else
error('Wrong # of arguments to semip_gc');
end;

results.order = order;
results.iter = iter;

% error checking on prior information inputs
[checkk,junk] = size(c);
if checkk ~= k
error('semip_gc: prior beta means are wrong');
elseif junk ~= 1
error('semip_gc: prior beta means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('semip_gc: prior bcov is wrong');
elseif junk ~= k
error('semip_gc: prior bcov is wrong');
end;

if pflag == 1
if ~isscalar(prho)
error('semip_gc: prior mean for rho should be a scalar');
end;
if ~isscalar(pvar)
error('semip_gc: prior variance for rho should be a scalar');
end;
end;

if eflag == 1; % Compute eigenvalues
t0 = clock;  
opt.tol = 1e-3; opt.disp = 0;
lambda = eigs(sparse(W),speye(m),1,'SR',opt);  
rmin = 1/lambda;   
rmax = 1;
time1 = etime(clock,t0);
end;

% do lndet calculations using 1 of 3 methods
switch lflag

case {0} % use full method, no approximations

t0 = clock;
out = lndetfull(W,rmin,rmax);
time2 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];

case{1} % use Pace and Barry, 1999 MC approximation

t0 = clock;
    out = lndetmc(order,iter,W);
time2 = etime(clock,t0);

results.limit = [out.rho out.lo95 out.lndet out.up95];
tt=.001:.001:1; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];

case{2} % use Pace and Barry, 1998 spline interpolation

t0 = clock;
out = lndetint(W);
time2 = etime(clock,t0);
tt=.001:.001:1; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];


otherwise
error('semip_gc: unrecognized lflag value on input');
% we should never get here

end; % end of different det calculation options

results.rmax = rmax; 
results.rmin = rmin;

% ====== initializations
% compute this once to save time

W = full(W);
TI = inv(T);
TIc = TI*c;
rho = p0;
inV = inV0;
sige = sig0;
a = a0;
ngrid = length(detval(:,1));
nsave = ndraw - nomit;


%*******************************
% Start GIBBS SAMPLER
%*******************************

t0 = clock;

if seedflag ~= 0;
rseed = num2str(seed);
end;


%************************************

% OUTPUTS: bdraw,adraw,pdraw,sdraw,rdraw,vmean,amean,zmean,yhat
% INPUTS:  y,x,W,ndraw,nomit,nsave,n,k,m,mobs,a,nu,d0,rval,mm,kk,detval,ngrid,TI,TIc,prho,pvar

disp('=== MCMC sampling ---');
if seedflag == 0
   [bdraw,adraw,pdraw,sdraw,rdraw,vmean,amean,zmean,yhat] = semip_gcc(...
   y,x,W,ndraw,nomit,nsave,n,k,m,mobs,a,nu,d0,rval,mm,kk,detval,ngrid,TI,TIc);
time3 = etime(clock,t0);
else 
   [bdraw,adraw,pdraw,sdraw,rdraw,vmean,amean,zmean,yhat] = semip_gcc(...
   y,x,W,ndraw,nomit,nsave,n,k,m,mobs,a,nu,d0,rval,mm,kk,detval,ngrid,TI,TIc,rseed);
time3 = etime(clock,t0);
end;    

time = etime(clock,timet);

% Save results

results.meth  = 'semip_g';
results.bdraw = bdraw;
results.adraw = adraw;
results.pdraw = pdraw;
results.sdraw = sdraw;
results.vmean = vmean;
results.amean = amean;
results.zmean = zmean;
results.yhat  = yhat;
results.bmean = c;
results.bstd = sqrt(diag(T));
results.pflag = pflag;
results.bflag = bflag;
results.eflag = eflag;
results.dflag = 0;

if mm~= 0
    results.rdraw = rdraw;
end

results.nobs  = n;
results.nvar  = k;
results.nreg  = m;
results.ndraw = ndraw;
results.nomit = nomit;
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.time  = time;
results.nu = nu;
results.d0 = d0;
results.tflag = 'plevel';
results.lflag = lflag;
results.m = m;
results.mobs = mobs;
if mm~= 0
results.rdraw = rsave;
results.m     = mm;
results.k     = kk;
else
results.r     = rval;
results.rdraw = 0;
end;
