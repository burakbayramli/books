function results = far_g(y,W,ndraw,nomit,prior)
% PURPOSE: Bayesian estimates for the 1st-order Spatial autoregressive model
%          y = rho*W*y + e,    e = N(0,sige*V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          sige = gamma(nu,d0)    
%          rho = Uniform(rmin,rmax), or rho = beta(a1,a2); 
%----------------------------------------------------------------
% USAGE: result =  far_g(y,W,ndraw,nomit,prior)
% where: y = nobs x 1 independent variable vector (mean = 0)
%        W = nobs x nobs 1st-order contiguity matrix (standardized)
%       ndraw = # of draws
%       nomit = # of initial draws omitted for burn-in
%       prior = a structure variable for prior information input
%        prior.novi  = 1 turns off sampling for vi, producing homoscedastic model. default = 0            
%        prior.nu,   = informative Gamma(nu,d0) prior on sige
%        prior.d0      default: nu=0,d0=0 (diffuse prior)
%        prior.a1    = parameter for beta(a1,a2) prior on rho (default = 1.01)
%        prior.a2    = (default = 1.01) see: 'help beta_prior'
%        prior.rval, = r prior hyperparameter, default=4
%        prior.m,    = informative Gamma(m,k) prior on r
%        prior.k,    = informative Gamma(m,k) prior on r
%        prior.eig   = 0 for default rmin = -1,rmax = +1, 1 for eigenvalue calculation of these
%        prior.rmin, = (optional) min value of rho to use in sampling
%        prior.rmax, = (optional) max value of rho to use in sampling
%        prior.lflag = 0 for full lndet computation (default = 1, fastest)
%                    = 1 for MC approximation (fast for very large problems)
%                    = 2 for Spline approximation (medium speed)
%        prior.dflag = 0 for numerical integration, 1 for Metropolis-Hastings (default = 0)
%        prior.order = order to use with info.lflag = 1 option (default = 50)
%        prior.iter  = iters to use with info.lflag = 1 option (default = 30)   
%        prior.lndet = a matrix returned by sar, far_g, sar_g, etc.
%                      containing log-determinant information to save time
%---------------------------------------------------------------
% RETURNS: a structure:
%          results.meth   = 'far_g'
%          results.pdraw  = rho draws (ndraw-nomit x 1)
%          results.sdraw  = sige draws (ndraw-nomit x 1)
%          results.vmean  = mean of vi draws (nobs x 1)
%          results.rdraw  = r-value draws (ndraw-nomit x 1)
%          results.nu     = prior nu-value for sige (if prior input)
%          results.d0     = prior d0-value for sige (if prior input)
%          results.a1     = a1 parameter for beta prior on rho from input, or default value
%          results.a2     = a2 parameter for beta prior on rho from input, or default value
%          results.r      = value of hyperparameter r (if input)
%          results.m      = m prior parameter (if input)
%          results.k      = k prior parameter (if input)    
%          results.nobs   = # of observations
%          results.ndraw  = # of draws
%          results.nomit  = # of initial draws omitted
%          results.y      = actual observations
%          results.yhat   = mean of posterior for y-predicted (nobs x 1)
%          results.time   = total time taken
%          results.time1  = time for log determinant calcluation
%          results.time2  = time for eigenvalue calculation   
%          results.time3  = time taken for sampling                 
%          results.rmax   = 1/max eigenvalue of W (or rmax if input)
%          results.rmin   = 1/min eigenvalue of W (or rmin if input) 
%          results.tflag  = 'plevel' (default) for printing p-levels
%                         = 'tstat' for printing bogus t-statistics      
%          results.lflag  = lflag from input
%          results.dflag  = dflag value from input (or default value used)
%          results.iter   = info.iter option from input
%          results.order  = info.order option from input
%          results.limit  = matrix of [rho lower95,logdet approx, upper95] intervals
%                           (for the case of lflag = 1)      
%          results.lndet = a matrix containing log-determinant information
%                          (for use in later function calls to save time)
%          results.acc   = acceptance rate for M-H sampling
%          results.mlike = log marginal likelihood (a vector ranging over
%                          rho values that can be integrated for model comparison)
%----------------------------------------------------------------
% NOTES: - use either improper prior.rval 
%          or informative Gamma prior.m, prior.k, not both of them
% - if you use lflag = 1 or 2, prior.rmin will be set = -1 
%                              prior.rmax will be set = 1
% - for n < 1000 you should use lflag = 0 to get exact results        
%----------------------------------------------------------------
% SEE ALSO: (far_gd, far_gd2, far_gd3, demos) prt, plt
% --------------------------------------------------------------
% REFERENCES: James P. LeSage, `Bayesian Estimation of Spatial Autoregressive
%             Models',  International Regional Science Review, 1997 
%             Volume 20, number 1\&2, pp. 113-129.
% For lndet information see: Ronald Barry and R. Kelley Pace, 
% "A Monte Carlo Estimator of the Log Determinant of Large Sparse Matrices", 
% Linear Algebra and its Applications", Volume 289, Number 1-3, 1999, pp. 41-54.
% and: R. Kelley Pace and Ronald P. Barry "Simulating Mixed Regressive
% Spatially autoregressive Estimators", 
% Computational Statistics, 1998, Vol. 13, pp. 397-418.
%----------------------------------------------------------------

% written by:
% James P. LeSage, last updated 8/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% NOTE: much of the speed for large problems comes from:
% the use of methods pioneered by Pace and Barry.
% R. Kelley Pace was kind enough to provide functions
% lndetmc, and lndetint from his spatial statistics toolbox
% for which I'm very grateful.

timet = clock; % start the timer

if (nargin < 4 | nargin > 5)
    error('far_g: Wrong # of input arguments');
end;

if nargin == 4
    prior.lflag = 1;
end;


 [n n2] = size(W);  
 if n ~= n2 % a non-square spatial weight matrix
 error('far_g: Wrong size 1st-order contiguity matrix');
 end; 
 [tst junk] = size(y);
 if tst ~= n % y-vector length doesn't match W-matrix
 error('far_g: Wrong size y vector on input');
 end;
 if junk ~= 1 % user didn't enter a column vector
 error('far_g: Wrong size y vector on input');
 end;

 
time1 = 0;
time2 = 0;
time3 = 0;

[nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval,ldetflag, ...
eflag,order,iter,novi_flag,cc,metflag,a1,a2] = far_parse(prior);

results.order = order;
results.iter = iter;



V = ones(n,1); in = ones(n,1); % initial value for V   
ys = y.*sqrt(V);
vi = in;
          
psave = zeros(ndraw-nomit,1);    % allocate storage for results
ssave = zeros(ndraw-nomit,1);
vmean = zeros(n,1);
yhat = zeros(n,1);
acc_rate = zeros(ndraw,1);


if mm~= 0                        % storage for draws on rvalue
rsave = zeros(ndraw-nomit,1);
end;

[rmin,rmax,time2] = far_eigs(eflag,W,rmin,rmax,n);

[detval,time1] = far_lndet(ldetflag,W,rmin,rmax,detval,order,iter);

iter = 1; 
time3 = clock; % start timing the sampler


% =====================================================
% The sampler starts here
% =====================================================

Wy = sparse(W)*y;
acc = 0;
cc = 0.1;

switch novi_flag    
case{0} % we do heteroscedastic model    
hwait = waitbar(0,'MCMC sampling far model ...');              

while (iter <= ndraw);        % start sampling;

% update sige;
nu1 = n + nu; 
Wys = sqrt(V).*Wy;
e = ys - rho*Wys;
d1 = d0 + e'*e;
chi = chis_rnd(1,nu1);
t2 = chi/d1;
sige = 1/t2;

% update vi
e = y - rho*Wy;
chiv = chis_rnd(n,rval+1);  
vi = ((e.*e./sige) + in*rval)./chiv;
V = in./vi;   
ys = y.*sqrt(V);

% update rval
if mm ~= 0           
rval = gamm_rnd(1,1,mm,kk);  
end;

     if metflag == 1
         % metropolis step to get rho update
          rhox = c_far(rho,y,sige,W,detval,in,a1,a2);
          accept = 0; 
          rho2 = rho + cc*randn(1,1); 
          while accept == 0
           if ((rho2 > rmin) & (rho2 < rmax)); 
           accept = 1;  
           else
           rho2 = rho + cc*randn(1,1);
           end; 
          end;
          rhoy = c_far(rho2,y,sige,W,detval,in,a1,a2);
          ru = unif_rnd(1,0,1);
          if ((rhoy - rhox) > exp(1)),
          p = 1;
          else, 
          ratio = exp(rhoy-rhox); 
          p = min(1,ratio);
          end;
              if (ru < p)
              rho = rho2;
              acc = acc + 1;
              end;
          rtmp(iter,1) = rho;
      acc_rate(iter,1) = acc/iter;
      % update cc based on std of rho draws
       if acc_rate(iter,1) < 0.4
       cc = cc/1.1;
       end;
       if acc_rate(iter,1) > 0.6
       cc = cc*1.1;
       end;

      end; % end of if metflag == 1


      if metflag == 0 % update rho using numerical integration
          e0 = ys;
          ed = Wys;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
          
          rho = draw_rho(detval,epe0,eped,epe0d,n,1,rho,a1,a2);
      end;

    if (iter > nomit)
    ssave(iter-nomit,1) = sige;
    psave(iter-nomit,1) = rho;
    vmean = vmean + vi;
     if mm~= 0
     rsave(iter-nomit,1) = rval;
     end; 
    end; % end of if > nomit
    
iter = iter+1;
waitbar(iter/ndraw);

end; % end of sampling loop
close(hwait);

time3 = etime(clock,time3);


% compute posterior means and log marginal likelihood for return arguments
rho = mean(psave);
sigm = mean(ssave);
vmean = vmean/(ndraw-nomit);
V = in./vmean;

          ys = sqrt(V).*y;
          Wys = sqrt(V).*Wy;
          e0 = ys;
          ed = Wys;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
e = (e0-rho*ed);
yhat = y - e;
sige = (1/n)*(e0-rho*ed)'*(e0-rho*ed); 
mlike = far_marginal(detval,e0,ed,epe0,eped,epe0d,n,1,a1,a2);


results.y = y;      
results.nobs = n;
results.nvar = 1;   
results.meth = 'far_g';
results.pdraw = psave;
results.sdraw = ssave;
results.vmean = vmean;
results.yhat = yhat;
results.resid = e;
results.tflag = 'plevel';
results.lflag = ldetflag;
results.dflag = metflag;
results.nobs  = n;
results.ndraw = ndraw;
results.nomit = nomit;
results.y = y;
results.nvar = 1;
results.mlike = mlike;
results.sige = sige;
results.rho = rho;
results.lndet = detval;
results.acc = acc_rate;
results.novi = novi_flag;

if mm~= 0
results.rdraw = rsave;
results.m     = mm;
results.k     = kk;
else
results.r     = rval;
results.rdraw = 0;
end;

results.time = etime(clock,timet);
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.lndet = detval;
results.rmax = rmax; 
results.rmin = rmin;

case{1} % we do homoscedastic model    
hwait = waitbar(0,'MCMC sampling far model ...');              

while (iter <= ndraw);        % start sampling;

% update sige;
nu1 = n + nu; 
e = y - rho*Wy;
d1 = d0 + e'*e;
chi = chis_rnd(1,nu1);
t2 = chi/d1;
sige = 1/t2;


     if metflag == 1
         % metropolis step to get rho update
          rhox = c_far(rho,y,sige,W,detval,in,a1,a2);
          accept = 0; 
          rho2 = rho + cc*randn(1,1); 
          while accept == 0
           if ((rho2 > rmin) & (rho2 < rmax)); 
           accept = 1;  
           else
           rho2 = rho + cc*randn(1,1);
           end; 
          end;
          rhoy = c_far(rho2,y,sige,W,detval,in,a1,a2);
          ru = unif_rnd(1,0,1);
          if ((rhoy - rhox) > exp(1)),
          p = 1;
          else, 
          ratio = exp(rhoy-rhox); 
          p = min(1,ratio);
          end;
              if (ru < p)
              rho = rho2;
              acc = acc + 1;
              end;
          rtmp(iter,1) = rho;
      acc_rate(iter,1) = acc/iter;
      % update cc based on std of rho draws
       if acc_rate(iter,1) < 0.4
       cc = cc/1.1;
       end;
       if acc_rate(iter,1) > 0.6
       cc = cc*1.1;
       end;

      end; % end of if metflag == 1


      if metflag == 0 % update rho using numerical integration
          e0 = y;
          ed = Wy;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
          
          rho = draw_rho(detval,epe0,eped,epe0d,n,1,rho,a1,a2);
      end;

    if (iter > nomit)
    ssave(iter-nomit,1) = sige;
    psave(iter-nomit,1) = rho;
     if mm~= 0
     rsave(iter-nomit,1) = rval;
     end; 
    end; % end of if > nomit
    
iter = iter+1;
waitbar(iter/ndraw);

end; % end of sampling loop
close(hwait);

time3 = etime(clock,time3);


% compute posterior means and log marginal likelihood for return arguments
rho = mean(psave);

          e0 = y;
          ed = Wy;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
e = (e0 - rho*ed);
yhat = y - e;
sige = (1/(n-1))*(e0-rho*ed)'*(e0-rho*ed); 
mlike = far_marginal(detval,e0,ed,epe0,eped,epe0d,n,1,a1,a2);

results.y = y;      
results.nobs = n;
results.nvar = 1;   
results.meth = 'far_g';
results.pdraw = psave;
results.sdraw = ssave;
results.vmean = vmean;
results.yhat = yhat;
results.resid = e;
results.tflag = 'plevel';
results.lflag = ldetflag;
results.dflag = metflag;
results.nobs  = n;
results.ndraw = ndraw;
results.nomit = nomit;
results.y = y;
results.nvar = 1;
results.mlike = mlike;
results.sige = sige;
results.rho = rho;
results.lndet = detval;
results.acc = acc_rate;
results.novi = novi_flag;


if mm~= 0
results.rdraw = rsave;
results.m     = mm;
results.k     = kk;
else
results.r     = rval;
results.rdraw = 0;
end;

results.time = etime(clock,timet);
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.lndet = detval;
results.rmax = rmax; 
results.rmin = rmin;


otherwise
error('far_g: unrecognized prior.novi_flag value on input');
% we should never get here

end; % end of homoscedastic vs. heteroscedastic vs. log-marginal options

% =========================================================================
% support functions below
% =========================================================================



function rho = draw_rho(detval,epe0,eped,epe0d,n,k,rho,a1,a2)
% update rho via univariate numerical integration

nmk = (n-k)/2;
nrho = length(detval(:,1));
iota = ones(nrho,1);

z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
den = detval(:,2) - nmk*log(z);
bprior = beta_prior(detval(:,1),a1,a2);
den = den + log(bprior);

n = length(den);
y = detval(:,1);
adj = max(den);
den = den - adj;
x = exp(den);

% trapezoid rule
isum = sum((y(2:n,1) + y(1:n-1,1)).*(x(2:n,1) - x(1:n-1,1))/2);
z = abs(x/isum);
den = cumsum(z);

rnd = unif_rnd(1,0,1)*sum(z);
ind = find(den <= rnd);
idraw = max(ind);
if (idraw > 0 & idraw < nrho)
rho = detval(idraw,1);
end;

function cout = c_far(rho,y,sige,W,detval,vi,a1,a2);
% PURPOSE: evaluate the conditional distribution of rho given sige
%  spatial autoregressive model using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:cout = c_far(rho,y,sige,W,detval,a1,a2)
%  where:  rho  = spatial autoregressive parameter
%          y    = dependent variable vector
%          W    = spatial weight matrix
%        detval = an (ngrid,2) matrix of values for det(I-rho*W) 
%                 over a grid of rho values 
%                 detval(:,1) = determinant values
%                 detval(:,2) = associated rho values
%          sige = sige value
%          a1    = (optional) prior parameter for rho
%          a2    = (optional) prior parameter for rho
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by far_g
%  --------------------------------------------------

gsize = detval(2,1) - detval(1,1);
% Note these are actually log detvalues
i1 = find(detval(:,1) <= rho + gsize);
i2 = find(detval(:,1) <= rho - gsize);
i1 = max(i1);
i2 = max(i2);
index = round((i1+i2)/2);
if isempty(index)
index = 1;
end;
detm = detval(index,2); 

n = length(y);
e = (speye(n) - rho*sparse(W))*y ;
ev = e.*sqrt(vi);
epe = (ev'*ev)/(2*sige);
bprior = beta_prior(detval(:,1),a1,a2);
epe = log(epe) + log(bprior);

cout =   detm - (n/2)*epe;



function [nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,novi_flag,cc,metflag,a1,a2] = far_parse(prior)
% PURPOSE: parses input arguments for far, far_g models
% ---------------------------------------------------
%  USAGE: [nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval, ...
%         ldetflag,eflag,mflag,order,iter,novi_flag,c,T,prior_beta,cc,metflag],a1,a2,switch_flag = 
%                           far_parse(prior,k)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% set defaults

eflag = 0;     % default to not computing eigenvalues
ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
mflag = 1;     % default to compute log marginal likelihood
order = 50;    % there are parameters used by the MC det approx
iter = 30;     % defaults based on Pace and Barry recommendation
rmin = -1;     % use -1,1 rho interval as default
rmax = 1;
detval = 0;    % just a flag
rho = 0.5;
sige = 1.0;
rval = 4;
mm = 0;
kk = 0;
nu = 0;
d0 = 0;
a1 = 1.01;
a2 = 1.01;
cc = 0.2;
novi_flag = 0; % do vi-estimates
metflag = 0; % use integration instead of M-H sampling for rho

fields = fieldnames(prior);
nf = length(fields);
if nf > 0
 for i=1:nf
    if strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;  
    elseif strcmp(fields{i},'rval')
        rval = prior.rval; 
    elseif strcmp(fields{i},'dflag')
       metflag = prior.dflag; 
    elseif strcmp(fields{i},'a1')
       a1 = prior.a1; 
    elseif strcmp(fields{i},'a2')
       a2 = prior.a2; 
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval   
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
        error('far_g: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = prior.order;  
    elseif strcmp(fields{i},'iter')
        iter = prior.iter; 
    elseif strcmp(fields{i},'novi')
        novi_flag = prior.novi;
    elseif strcmp(fields{i},'dflag')
        metflag = prior.dflag;
    elseif strcmp(fields{i},'eig')
        eflag = prior.eig;
    elseif strcmp(fields{i},'mlog')
        mlog = prior.mlog;
    end;
 end;

else, % the user has input a blank info structure
      % so we use the defaults
end; 


function [rmin,rmax,time2] = far_eigs(eflag,W,rmin,rmax,n);
% PURPOSE: compute the eigenvalues for the weight matrix
% ---------------------------------------------------
%  USAGE: [rmin,rmax,time2] = far_eigs(eflag,W,rmin,rmax,W)
% where eflag is an input flag, W is the weight matrix
%       rmin,rmax may be used as default outputs
% and the outputs are either user-inputs or default values
% ---------------------------------------------------


if eflag == 1
t0 = clock;
opt.tol = 1e-3; opt.disp = 0;
lambda = eigs(sparse(W),speye(n),1,'SR',opt);  
rmin = 1/lambda;   
rmax = 1;
time2 = etime(clock,t0);
else
time2 = 0;
end;


function [detval,time1] = far_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
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
            error('far_g: wrgon lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('far_g: wrong sized lndet input argument');
        elseif n1 == 1
            error('far_g: wrong sized lndet input argument');
        end;          
end;

function  out = far_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,a1,a2)
% PURPOSE: returns a vector of the log-marginal over a grid of rho-values
% -------------------------------------------------------------------------
% USAGE: out = far_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,a1,a2)
% where:       detval = an ngrid x 2 matrix with rho-values and lndet values
%                  e0 = y;
%                 ed = Wy;
%               epe0 = e0'*e0;
%               eped = ed'*ed;
%              epe0d = ed'*e0;
%               nobs = # of observations
%               nvar = # of explanatory variables
%                 a1 = parameter for beta prior on rho
%                 a2 = parameter for beta prior on rho
% -------------------------------------------------------------------------
% RETURNS: out = a structure variable
%        out = log marginal, a vector the length of detval
% -------------------------------------------------------------------------

% written by:
% James P. LeSage, 7/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

n = length(detval);
nmk = (nobs-nvar)/2;
bprior = beta_prior(detval(:,1),a1,a2);
C = log(bprior) + gammaln(nmk) - nmk*log(2*pi);
iota = ones(n,1);
z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
den =  detval(:,2) - nmk*log(z);
den = real(den);
out = C + den;
