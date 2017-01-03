function results = semp_g(y,x,W,ndraw,nomit,prior)
% PURPOSE: Bayesian estimates of the spatial probit error model
%          y = XB + u, u = rho*W + e
%          y = binary, 0,1 variable
%          e = N(0,1),  
%          B = N(c,T), 
%          1/sige = Gamma(nu,d0), 
%          rho = Uniform(rmin,rmax), or rho = beta(a1,a2); 
%-------------------------------------------------------------
% USAGE: results = semp_g(y,x,W,ndraw,nomit,prior)
% where: y = binary dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%        W = spatial weight matrix (standardized, row-sums = 1)
%    ndraw = # of draws
%    nomit = # of initial draws omitted for burn-in            
%    prior = a structure variable with:
%            prior.beta  = prior means for beta,   c above (default 0)
%            priov.bcov  = prior beta covariance , T above (default 1e+12)
%            prior.nu    = informative Gamma(nu,d0) prior on sige
%            prior.d0    = default: nu=0,d0=0 (diffuse prior)
%            prior.a1    = parameter for beta(a1,a2) prior on rho (default = 1.01)
%            prior.a2    = (default = 1.01) see: 'help beta_prior'
%            prior.rmin  = (optional) min rho used in sampling (default = -1)
%            prior.rmax  = (optional) max rho used in sampling (default = +1)  
%            prior.eigs  = 0 to compute rmin/rmax using eigenvalues, (1 = don't compute default)
%            prior.lflag = 0 for full lndet computation (default = 1, fastest)
%                        = 1 for MC approx (fast for large problems)
%                        = 2 for Spline approx (medium speed)
%            prior.order = order to use with prior.lflag = 1 option (default = 50)
%            prior.iter  = iters to use with prior.lflag = 1 option (default = 30)   
%            prior.lndet = a matrix returned by sar, sar_g, sarp_g, etc.
%                          containing log-determinant information to save time
%-------------------------------------------------------------
% RETURNS:  a structure:
%          results.meth   = 'semp_g'
%          results.beta   = posterior mean of bhat
%          results.rho    = posterior mean of rho
%          results.sige   = posterior mean of sige
%          results.bdraw  = bhat draws (ndraw-nomit x nvar)
%          results.pdraw  = rho  draws (ndraw-nomit x 1)
%          results.sdraw  = sige draws (ndraw-nomit x 1)
%          results.bmean  = b prior means, prior.beta from input
%          results.bstd   = b prior std deviations sqrt(diag(prior.bcov))
%          results.nobs   = # of observations
%          results.nvar   = # of variables in x-matrix
%          results.ndraw  = # of draws
%          results.nomit  = # of initial draws omitted
%          results.y      = y-vector from input (nobs x 1)
%          results.zip    = # of zero y-values
%          results.yhat   = mean of posterior predicted (nobs x 1)
%          results.resid  = residuals, based on posterior means
%          results.rsqr   = r-squared based on posterior means
%          results.rbar   = adjusted r-squared
%          results.nu     = nu prior parameter
%          results.d0     = d0 prior parameter
%          results.a1     = a1 parameter for beta prior on rho from input, or default value
%          results.a2     = a2 parameter for beta prior on rho from input, or default value
%          results.time1  = time for eigenvalue calculation
%          results.time2  = time for log determinant calcluation
%          results.time3  = time for sampling
%          results.time   = total time taken  
%          results.rmax   = 1/max eigenvalue of W (or rmax if input)
%          results.rmin   = 1/min eigenvalue of W (or rmin if input)          
%          results.tflag  = 'plevel' (default) for printing p-levels
%                         = 'tstat' for printing bogus t-statistics 
%          results.lflag  = lflag from input
%          results.iter   = prior.iter option from input
%          results.order  = prior.order option from input
%          results.limit  = matrix of [rho lower95,logdet approx, upper95] 
%                           intervals for the case of lflag = 1
%          results.lndet = a matrix containing log-determinant information
%                          (for use in later function calls to save time)
%          results.mlike = log marginal likelihood for model comparisons
%                          (a vector ranging over rho-values from rmin to rmax that can be
%                          integrated for model comparison)
%          results.acc   = acceptance rate for M-H sampling (ndraw x 1) vector
% --------------------------------------------------------------
% NOTES: 
% - for n < 500 you should use lflag = 0 to get exact results  
% - use a1 = 1.0 and a2 = 1.0 for uniform prior on rho
% --------------------------------------------------------------
% SEE ALSO: (semp_gd, semp_gd2 demos) prt
% --------------------------------------------------------------
% REFERENCES: James P. LeSage, "Bayesian Estimation of Limited Dependent
%             variable Spatial Autoregressive Models", 
%             Geographical Analysis, 2000, Vol. 32, pp. 19-35.
% For lndet information see: Ronald Barry and R. Kelley Pace, 
% "A Monte Carlo Estimator of the Log Determinant of Large Sparse Matrices", 
% Linear Algebra and its Applications", Volume 289, Number 1-3, 1999, pp. 41-54.
% and: R. Kelley Pace and Ronald P. Barry 
% "Simulating Mixed Regressive Spatially autoregressive Estimators", 
% Computational Statistics, 1998, Vol. 13, pp. 397-418.
%----------------------------------------------------------------

% written by:
% James P. LeSage, 12/2001, updated 11/2003
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
yin = y;
results.y = y;
[n1 k] = size(x);
[n3 n4] = size(W);
time1 = 0;
time2 = 0;
time3 = 0;

if n1 ~= n
error('semp_g: x-matrix contains wrong # of observations');
elseif n3 ~= n4
error('semp_g: W matrix is not square');
elseif n3~= n
error('semp_g: W matrix is not the same size at y,x');
end;

if nargin == 5
    prior.lflag = 1;
end;

[nu,d0,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,novi_flag,c,T,cc,metflag,a1,a2,inform_flag] = semp_parse(prior,k);

results.order = order;
results.iter = iter;

% error checking on prior information inputs
[checkk,junk] = size(c);
if checkk ~= k
error('semp_g: prior means are wrong');
elseif junk ~= 1
error('semp_g: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('semp_g: prior bcov is wrong');
elseif junk ~= k
error('semp_g: prior bcov is wrong');
end;

         
bsave = zeros(ndraw-nomit,1);    % allocate storage for results
ssave = zeros(ndraw-nomit,1);
yhat = zeros(n,1);


[rmin,rmax,time1] = sem_eigs(eflag,W,rmin,rmax,n);

results.rmin = rmin;
results.rmax = rmax;
results.lflag = ldetflag;
results.time1 = time1;

[detval,time2] = sem_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
results.time2 = time3;

% storage for draws
          bsave = zeros(ndraw-nomit,k);
          psave = zeros(ndraw-nomit,1);
          ssave = zeros(ndraw-nomit,1);
          ymean = zeros(n,1);
          acc_rate = zeros(ndraw,1);

% ====== initializations
% compute this stuff once to save time
TI = inv(T);
TIc = TI*c;
iter = 1;
in = ones(n,1);
Wy = sparse(W)*y;
Wx = sparse(W)*x;


% find an index of values = 0
zipv = find(yin == 0);
zipo = find(yin == 1);
nzip = length(zipv);
W2diag = spdiags(W'*W,0);



hwait = waitbar(0,'semp\_g: MCMC sampling ...');
t0 = clock;                  
iter = 1;
acc = 0;
          while (iter <= ndraw); % start sampling;
                  
          % update beta   
          xs = x - rho*Wx;
          AI = inv(xs'*xs + sige*TI);
          ys = y - rho*Wy;
          b = xs'*ys + sige*TIc;
          b0 = AI*b;
          bhat = norm_rnd(sige*AI) + b0; 
            
          % update sige
          nu1 = n + 2*nu; 
          e = ys-xs*bhat;
          d1 = 2*d0 + e'*e;
          chi = chis_rnd(1,nu1);
          sige = d1/chi;
              
          
         % update z-values
          mu = x*bhat;
          ymu = y - mu;
           dsig = ones(n,1) - rho*rho*W2diag;
           yvar = ones(n,1)./dsig;
           A =  (1/sige)*(speye(n)-rho*W)*ymu; % a vector
           B =  (speye(n)-rho*W)'*A;  % a vector
           Cy = ymu - yvar.*B ;
           ym = mu + Cy;
          
           ind = find(yin == 0);
           y(ind,1) = normrt_rnd(ym(ind,1),yvar(ind,1),0);
            
           ind = find(yin == 1);
           y(ind,1) = normlt_rnd(ym(ind,1),yvar(ind,1),0);
         
          % reformulate Wy
          Wy = sparse(W)*y;
         
          % update rho using metropolis-hastings
          % numerical integration is too slow here
          xb = x*bhat;
          rhox = c_sem(rho,y,x,bhat,sige,W,detval,ones(n,1),a1,a2);
          accept = 0;
          rho2 = rho + cc*randn(1,1);
          while accept == 0
           if ((rho2 > rmin) & (rho2 < rmax)); 
           accept = 1;  
           else
           rho2 = rho + cc*randn(1,1);
           end; 
          end; 
           rhoy = c_sem(rho2,y,x,bhat,sige,W,detval,ones(n,1),a1,a2);
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
      acc_rate(iter,1) = acc/iter;
      % update cc based on std of rho draws
       if acc_rate(iter,1) < 0.4
       cc = cc/1.1;
       end;
       if acc_rate(iter,1) > 0.6
       cc = cc*1.1;
       end;

                                                               
    if iter > nomit % if we are past burn-in, save the draws
    bsave(iter-nomit,1:k) = bhat';
    ssave(iter-nomit,1) = sige;
    psave(iter-nomit,1) = rho;
    ymean = ymean + y;
    end;
                    

iter = iter + 1; 
waitbar(iter/ndraw);         
end; % end of sampling loop
close(hwait);

ymean = ymean/(ndraw-nomit);
Wy = sparse(W)*ymean;

time3 = etime(clock,t0);

% find posterior means and compute log-marginal
bmean = mean(bsave);
bmean = bmean';
rho = mean(psave);

[nobs,nvar] = size(x);
% compute log marginal likelihood for model comparisions
if inform_flag == 0
mlike = sem_marginal(detval,ymean,x,Wy,Wx,nobs,nvar,a1,a2);
else
mlike = sem_marginal2(detval,ymean,x,Wy,Wx,nobs,nvar,a1,a2,c,TI,sige);
end;

yhat = x*bmean;
yprob = stdn_cdf(yhat);
y = results.y;
n = length(y);
e = ymean-x*bmean;
sigu = (e'*e);
sige = sigu/(nobs-nvar);
ym = ymean - mean(ymean);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % conventional r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);

time = etime(clock,timet);

results.meth  = 'semp_g';
results.beta = bmean;
results.rho = rho;
results.sige = sige;
results.bdraw = bsave;
results.pdraw = psave;
results.sdraw = ssave;
results.yhat  = yhat;
results.yprob = yprob;
results.ymean = ymean;
results.bmean = c;
results.bstd  = sqrt(diag(T));
results.rsqr  = rsqr;
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
results.sige = sige;
results.nobs  = n;
results.zip = nzip;
results.nvar  = nvar;
results.ndraw = ndraw;
results.nomit = nomit;
results.time  = time;
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.acc = acc_rate;
results.dflag = metflag;
results.nu = nu;
results.d0 = d0;
results.a1 = a1;
results.a2 = a2;
results.mlike = mlike;
results.tflag = 'plevel';
results.novi = novi_flag;
results.lndet = detval;
results.priorb = inform_flag;



% =========================================================================
% support functions are below
% =========================================================================

function rho = draw_rho(detval,y,x,Wy,Wx,V,n,k,rmin,rmax,rho)
% update rho via univariate numerical integration
% for the heteroscedastic model case

nmk = (n-k)/2;
rgrid = rmin+0.01:0.1:rmax-0.01;
ng = length(rgrid);
epet = zeros(ng,1);
detxt = zeros(ng,1);
for i=1:ng;
xs = x - rgrid(i)*Wx;
xs = matmul(xs,sqrt(V));
ys = y - rgrid(i)*Wy;
ys = ys.*sqrt(V);
bs = (xs'*xs)\(xs'*ys);
e = ys - xs*bs;
epet(i,1) = e'*e;
%detxt(i,1) = det(xs'*xs);
end;

% interpolate a finer grid
epe = interp1(rgrid',epet,detval(:,1),'spline');
%detx = interp1(rgrid',detxt,detval(:,1),'spline');

%den = detval(:,2) -0.5*log(detx) - nmk*log(epe);
den = detval(:,2) - nmk*log(epe);

adj = max(den);
den = den - adj;
den = exp(den);

n = length(den);
y = detval(:,1);
x = den;

% trapezoid rule
isum = sum((y(2:n,1) + y(1:n-1,1)).*(x(2:n,1) - x(1:n-1,1))/2);

z = abs(x/isum);
den = cumsum(z);
nrho = length(detval(:,1));
rnd = unif_rnd(1,0,1)*sum(z);
ind = find(den <= rnd);
idraw = max(ind);
if (idraw > 0 & idraw < nrho)
rho = detval(idraw,1);
end;



function cout = c_sem(rho,y,x,b,sige,W,detval,vi,a1,a2);
% PURPOSE: evaluate the conditional distribution of rho given sige
%  spatial autoregressive model using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:cout = c_sar(rho,y,x,b,sige,W,detval,a1,a2)
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
%  NOTE: called only by sar_g
%  --------------------------------------------------
%  SEE ALSO: sar_g, c_far, c_sac, c_sem
% ---------------------------------------------------

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
z = speye(n) - rho*sparse(W);
 e = z*y - z*x*b;
 ev = e.*sqrt(vi);
 epe = (ev'*ev)/(2*sige);

cout =   detm - epe;


function [nu,d0,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,novi_flag,c,T,cc,metflag,a1,a2,inform_flag] = semp_parse(prior,k)
% PURPOSE: parses input arguments for semp models
% ---------------------------------------------------
%  USAGE: [nu,d0,rho,sige,rmin,rmax,detval, ...
%          ldetflag,eflag,order,iter,novi_flag,c,T,prior_beta,cc,metflag,a1,a2,inform_flag] = 
%                           semp_parse(prior,k)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% set defaults

eflag = 1;     % default to not computing eigenvalues
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
c = zeros(k,1);   % diffuse prior for beta
T = eye(k)*1e+12;
novi_flag = 0;    % default is do vi-estimates
metflag = 1;      % default to Metropolis-Hasting sampling
cc = 0.2;         % initial tuning parameter for M-H sampling
inform_flag = 0;  % flag for diffuse prior on beta


fields = fieldnames(prior);
nf = length(fields);
if nf > 0
 for i=1:nf
    if strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;  
    elseif strcmp(fields{i},'eigs')
        eflag = prior.eigs;
    elseif strcmp(fields{i},'dflag')
        metflag = prior.dflag;
    elseif strcmp(fields{i},'a1')
       a1 = prior.a1; 
    elseif strcmp(fields{i},'a2')
       a2 = prior.a2; 
    elseif strcmp(fields{i},'beta')
        c = prior.beta; inform_flag = 1; % flag for informative prior on beta
    elseif strcmp(fields{i},'bcov')
        T = prior.bcov; inform_flag = 1; % flag for informative prior on beta
    elseif strcmp(fields{i},'rmin')
        rmin = prior.rmin;  
    elseif strcmp(fields{i},'rmax')
        rmax = prior.rmax; 
    elseif strcmp(fields{i},'lndet')
    detval = prior.lndet;
    ldetflag = -1;
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
        error('semp_g: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = prior.order;  
    elseif strcmp(fields{i},'iter')
        iter = prior.iter; 
    elseif strcmp(fields{i},'novi')
        novi_flag = prior.novi;
    end;
 end;
 
else, % the user has input a blank info structure
      % so we use the defaults
end; 


function [rmin,rmax,time2] = sem_eigs(eflag,W,rmin,rmax,n);
% PURPOSE: compute the eigenvalues for the weight matrix
% ---------------------------------------------------
%  USAGE: [rmin,rmax,time2] = far_eigs(eflag,W,rmin,rmax,W)
% where eflag is an input flag, W is the weight matrix
%       rmin,rmax may be used as default outputs
% and the outputs are either user-inputs or default values
% ---------------------------------------------------


if eflag == 0
t0 = clock;
opt.tol = 1e-3; opt.disp = 0;
lambda = eigs(sparse(W),speye(n),1,'SR',opt);  
rmin = 1/lambda;   
rmax = 1;
time2 = etime(clock,t0);
else
time2 = 0;
end;


function [detval,time1] = sem_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
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
            error('semp_g: wrgon lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('semp_g: wrong sized lndet input argument');
        elseif n1 == 1
            error('semp_g: wrong sized lndet input argument');
        end;          
end;


function  out = sem_marginal(detval,y,x,Wy,Wx,nobs,nvar,a1,a2)
% PURPOSE: returns a vector of the log-marginal over a grid of rho-values
% -------------------------------------------------------------------------
% USAGE: out = sem_marginal(detval,y,x,Wy,Wx,nobs,nvar,a1,a2)
% where:       detval = an ngrid x 2 matrix with rho-values and lndet values
%                  y = y-vector
%                  x = x-matrix
%                 Wy = W*y-vector
%                 Wx = W*x-matrix
%               nobs = # of observations
%               nvar = # of explanatory variables
%                 a1 = parameter for beta prior on rho
%                 a2 = parameter for beta prior on rho
% -------------------------------------------------------------------------
% RETURNS: out = a structure variable
%        out.log = log marginal, a vector the length of detval
%        out.lik = concentrated log-likelihood vector the length of detval
% -------------------------------------------------------------------------
% NOTES: works only for homoscedastic SEM model
% we must feed in ys = sqrt(V)*y, xs = sqrt(V)*X 
% as well as logdetx = log(xs'*xs) for heteroscedastic model
% -------------------------------------------------------------------------

% written by:
% James P. LeSage, 7/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

nmk = (nobs-nvar)/2;

nrho = length(detval(:,1));
iota = ones(nrho,1);
rvec = detval(:,1);
epe = zeros(nrho,1);
rgrid = detval(1,1)+0.001:0.1:detval(end,1)-0.001;
rgrid = rgrid';

epetmp = zeros(length(rgrid),1);
detxtmp = zeros(length(rgrid),1);
for i=1:length(rgrid);
xs = x - rgrid(i,1)*Wx;
ys = y - rgrid(i,1)*Wy;
bs = (xs'*xs)\(xs'*ys);
e = ys - xs*bs;
epetmp(i,1) = e'*e;
detxtmp(i,1) = det(xs'*xs);
end;

% spline interpolate epetmp
tt=rvec; % interpolate a finer grid
epe = interp1(rgrid,epetmp,rvec,'spline'); 
detx = interp1(rgrid,detxtmp,rvec,'spline'); 


bprior = beta_prior(detval(:,1),a1,a2);
% C is a constant of integration that can vary with nvars, so for model
% comparisions involving different nvars we need to include this
C = log(bprior) + gammaln(nmk) - nmk*log(2*pi) ;
den = detval(:,2) - 0.5*log(detx) - nmk*log(epe);
den = real(den);
out = den + C;

function  out = sem_marginal2(detval,y,x,Wy,Wx,nobs,nvar,a1,a2,c,TI,sige)
% PURPOSE: returns a vector of the log-marginal over a grid of rho-values
%          for the case of an informative prior on beta
% -------------------------------------------------------------------------
% USAGE: out = sem_marginal2(detval,y,x,Wy,Wx,nobs,nvar,a1,a2,c,TI,sige)
% where:       detval = an ngrid x 2 matrix with rho-values and lndet values
%                  y = y-vector
%                  x = x-matrix
%                 Wy = W*y-vector
%                 Wx = W*x-matrix
%               nobs = # of observations
%               nvar = # of explanatory variables
%                 a1 = parameter for beta prior on rho
%                 a2 = parameter for beta prior on rho
% -------------------------------------------------------------------------
% RETURNS: out = a structure variable
%        out.log = log marginal, a vector the length of detval
%        out.lik = concentrated log-likelihood vector the length of detval
% -------------------------------------------------------------------------
% NOTES: works only for homoscedastic SEM model
% we must feed in ys = sqrt(V)*y, xs = sqrt(V)*X 
% as well as logdetx = log(xs'*xs) for heteroscedastic model
% -------------------------------------------------------------------------

% written by:
% James P. LeSage, 7/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

nmk = (nobs-nvar)/2;

nrho = length(detval(:,1));
iota = ones(nrho,1);
rvec = detval(:,1);
epe = zeros(nrho,1);

rgrid = detval(1,1)+0.001:0.1:detval(end,1)-0.001;
rgrid = rgrid';
epetmp = zeros(length(rgrid),1);
detxtmp = zeros(length(rgrid),1);
Q1 = zeros(length(rgrid),1);
Q2 = zeros(length(rgrid),1);
sTI = sige*TI;

for i=1:length(rgrid);
xs = x - rgrid(i,1)*Wx;
ys = y - rgrid(i,1)*Wy;
bs = (xs'*xs)\(xs'*ys);
beta = (xs'*xs + sTI)\(xs'*ys + sTI*c);
e = ys - xs*bs;
epetmp(i,1) = e'*e;
detxtmp(i,1) = det(xs'*xs);
Q1(i,1) = (c - beta)'*sTI*(c - beta);
Q2(i,1) = (bs - beta)'*(xs'*xs)*(bs - beta);
end;

% spline interpolate epetmp
tt=rvec; % interpolate a finer grid
epe = interp1(rgrid,epetmp,rvec,'spline'); 
detx = interp1(rgrid,detxtmp,rvec,'spline'); 
Q1 = interp1(rgrid,Q1,rvec,'spline'); 
Q2 = interp1(rgrid,Q2,rvec,'spline'); 
bprior = beta_prior(detval(:,1),a1,a2);
% C is a constant of integration that can vary with nvars, so for model
% comparisions involving different nvars we need to include this
C = log(bprior) + gammaln(nmk) - nmk*log(2*pi) ;
den = detval(:,2) - 0.5*log(detx) - nmk*log(epe + Q1 + Q2);
den = real(den);
out = den + C;


function rho = olddraw_rho(detval,y,x,Wy,Wx,V,n,k,rmin,rmax,rho)
% update rho via univariate numerical integration
% for the heteroscedastic model case

nmk = (n-k)/2;
nrho = length(detval(:,1));
iota = ones(nrho,1);
rvec = detval(:,1);
epe = zeros(nrho,1);

for i=1:nrho;
xs = x - rvec(i,1)*Wx;
xs = matmul(xs,sqrt(V));
ys = y - rvec(i,1)*Wy;
ys = ys.*sqrt(V);
bs = (xs'*xs)\(xs'*ys);
e = ys - xs*bs;
epe(i,1) = e'*e;
end;

den = detval(:,2) - nmk*log(epe);
adj = max(den);
den = den - adj;
den = exp(den);


n = length(den);
y = detval(:,1);
x = den;

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
