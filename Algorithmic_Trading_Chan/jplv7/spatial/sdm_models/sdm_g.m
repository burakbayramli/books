function results = sdm_g(y,x,W,ndraw,nomit,prior)
% PURPOSE: Bayesian estimates of the spatial Durbin model
%          y = rho*W*y + a + X*b1 + W*X*b2 + e, e = N(0,sige*V), V = diag(v1,v2,...vn) 
%          r/vi = ID chi(r)/r, r = Gamma(m,k)
%          a,b1,b2 = N(prior.beta,prior.bcov)
%          1/sige = Gamma(nu,d0), 
%          rho = Uniform(rmin,rmax), or rho = beta(a1,a2); 
%          a1 = a2 = 1 produces uniform prior
%-------------------------------------------------------------
% USAGE: results = sdm_g(y,x,W,ndraw,nomit,prior)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar), 
%            with constant term in 1st column (if used)
%        W = spatial weight matrix (standardized, row-sums = 1)
%    ndraw = # of draws
%    nomit = # of initial draws omitted for burn-in            
%    prior = a structure variable with:
%            prior.beta  = prior means for beta,   (default 0)
%            prior.bcov  = prior beta covariance , (default eye(nvars)*1e+12)
%            prior.rval  = r prior hyperparameter, default = 4
%            prior.novi  = 1 turns off sampling for vi, producing homoscedastic model V = eye(nobs)           
%            prior.m     = informative Gamma(m,k) prior on r
%            prior.k     = (default: not used)
%            prior.nu    = informative Gamma(nu,d0) prior on sige
%            prior.d0    = default: nu=0,d0=0 (diffuse prior)
%            prior.a1    = parameter for beta(a1,a2) prior on rho see: 'help beta_prior'
%            prior.a2    = (default = 1.0, a uniform prior on rmin,rmax) 
%            prior.eig   = 0 for default rmin = -1,rmax = +1, 1 for eigenvalue calculation of these
%            prior.rmin  = (optional) min rho used in sampling (default = -1)
%            prior.rmax  = (optional) max rho used in sampling (default = 1)  
%            prior.lflag = 0 for full lndet computation (default = 1, fastest)
%                        = 1 for MC approx (fast for large problems)
%                        = 2 for Spline approx (medium speed)
%            prior.order = order to use with prior.lflag = 1 option (default = 50)
%            prior.iter  = iters to use with prior.lflag = 1 option (default = 30) 
%            prior.lndet = a matrix returned by sar, sar_g, sarp_g, etc.
%                          containing log-determinant information to save time
%            prior.logm  = 0 for no log marginal calculation, = 1 for log marginal (default = 1)
%-------------------------------------------------------------
% RETURNS:  a structure variable with fields:
%        results.meth   = 'sdm_g'
%        results.beta   = posterior mean of bhat based on draws
%        results.rho    = posterior mean of rho based on draws
%        results.sige   = posterior mean of sige based on draws
%        results.beta_std = std deviation of beta draws
%        results.sige_std = std deviation of sige draws
%        results.rho_std  = std deviation of rho draws
%        results.total  = a 3-d matrix (ndraw-nomit,nvars-1,101) total x-impacts
%                           (ndraw-nomit,nvars,101) if no constant term in the model
%        results.direct = a 3-d matrix (ndraw-nomit,nvars-1,ntrs) direct x-impacts
%                           (ndraw-nomit,nvars,101) if no constant term in the model
%        results.indirect = a 3-d matrix (ndraw-nomit,nvars-1,ntrs) indirect x-impacts
%                           (ndraw-nomit,nvars,101) if no constant term in the model
%        results.sigma  = posterior mean of sige based on (e'*e)/(n-k)
%        results.bdraw  = bhat draws (ndraw-nomit x nvar)
%        results.pdraw  = rho  draws (ndraw-nomit x 1)
%        results.sdraw  = sige draws (ndraw-nomit x 1)
%        results.vmean  = posterior mean of vi draws (nobs x 1) 
%        results.rdraw  = r draws (ndraw-nomit x 1) (if m,k input)
%        results.bmean  = b prior means, prior.beta from input
%        results.bstd   = b prior std deviations sqrt(diag(prior.bcov))
%        results.r      = value of hyperparameter r (if input)
%        results.novi   = 1 for prior.novi = 1, 0 for prior.rval input
%        results.nobs   = # of observations
%        results.nvar   = # of variables in [iota x W*x]
%        results.ndraw  = # of draws
%        results.nomit  = # of initial draws omitted
%        results.cflag  = 0 for no intercept, = 1 for intercept term
%        results.p      = # of variables in x-matrix (excluding constant
%                           term if used)
%        results.y      = y-vector from input (nobs x 1)
%        results.yhat   = mean of posterior predicted (nobs x 1) inv(I-rho*W)*x*bhat
%        results.resid  = residuals, based on posterior means, y - yhat
%        results.rsqr   = r-squared based on posterior means (using conventional formula)
%        results.rbar   = adjusted r-squared
%        results.nu     = nu prior parameter for gamma(nu,d0) prior on sige noise variance
%        results.d0     = d0 prior parameter for gamma(nu,d0) prior on sige noise variance
%        results.a1     = a1 parameter for beta prior on rho from input, or default value
%        results.a2     = a2 parameter for beta prior on rho from input, or default value 1.01
%        results.time1  = time for eigenvalue calculation
%        results.time2  = time for log determinant calcluation
%        results.time3  = time for sampling
%        results.time4  = time taken calculating effects estimates
%        results.time   = total time taken  
%        results.rmax   = 1/max eigenvalue of W (or rmax if input)
%        results.rmin   = 1/min eigenvalue of W (or rmin if input)          
%        results.tflag  = 'plevel' (default) for printing p-levels
%                       = 'tstat' for printing bogus t-statistics 
%        results.lflag  = lflag from input
%        results.iter   = prior.iter option from input
%        results.order  = prior.order option from input
%        results.limit  = matrix of [rho lower95,logdet approx, upper95] 
%                         intervals for the case of lflag = 1
%        results.lndet = a matrix containing log-determinant information
%                        (for use in later function calls to save time)
%        results.mlike = log marginal likelihood (a vector ranging over
%                        rho values that can be integrated for model comparison)
% --------------------------------------------------------------
% NOTES: constant term should be in 1st column of the x-matrix
%        constant is excluded from b2 estimates
% - use a1 = 1.0 and a2 = 1.0 for uniform prior on rho
% --------------------------------------------------------------
% SEE ALSO: (sdm_gd, sdm_gd2 demos) 
% --------------------------------------------------------------
% REFERENCES: LeSage and Pace (2009) Introduction to Spatial Econometrics
% Chapter 5 on Bayesian spatial regression models.
% For lndet information see: Chapter 4
% For interpretation of direct, indirect and total x-impacts see: Chapter 2
%----------------------------------------------------------------

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com

timet = clock;
time1 = 0;
time2 = 0;
time3 = 0;
time4 = 0;

% check if the user handled the intercept term okay
n = length(y);
if sum(x(:,1)) ~= n
tst = sum(x); % we may have no intercept term
ind = find(tst == n); % we do have an intercept term
 if length(ind) > 0
 error('sdm: intercept term must be in first column of the x-matrix');
 elseif length(ind) == 0 % case of no intercept term
 xsdm = [x W*x];
 cflag = 0;
 p = size(x,2);
 end;
elseif sum(x(:,1)) == n % we have an intercept in the right place
 xsdm = [x W*x(:,2:end)];
 cflag = 1;
 p = size(x,2)-1;
end;

[nobs,k] = size(xsdm);


results.nobs  = n;
results.nvar  = k;
results.y = y; 

if nargin == 5
    prior.lflag = 1;
end;
  
[nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,novi_flag,c,T,inform_flag,a1,a2] = sdm_parse(prior,k);


% check if the user handled the intercept term okay
    n = length(y);
    if sum(x(:,1)) ~= n
    tst = sum(x); % we may have no intercept term
    ind = find(tst == n); % we do have an intercept term
     if length(ind) > 0
     error('sdm_g: intercept term must be in first column of the x-matrix');
     elseif length(ind) == 0 % case of no intercept term
     cflag = 0;
     p = size(x,2);
     end;
    elseif sum(x(:,1)) == n % we have an intercept in the right place
     cflag = 1;
     p = size(x,2)-1;
    end;
     
    [n1,n2] = size(W);
    if n1 ~= n2
    error('sdm_g: wrong size weight matrix W');
    elseif n1 ~= n
    error('sdm: wrong size weight matrix W');
    end;
    [nchk junk] = size(y);
    if nchk ~= n
    error('sdm_g: wrong size y vector input');
    end;

    % error checking on prior information inputs
    [checkk,junk] = size(c);
    if checkk ~= k
    error('sdm_g: prior means are wrong');
    elseif junk ~= 1
    error('sdm_g: prior means are wrong');
    end;

    [checkk junk] = size(T);
    if checkk ~= k
    error('sdm_g: prior bcov is wrong');
    elseif junk ~= k
    error('sdm_g: prior bcov is wrong');
    end;


results.order = order;
results.iter = iter;

[rmin,rmax,time1] = sdm_eigs(eflag,W,rmin,rmax,n);
results.time1 = time1;

[detval,time2] = sdm_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
results.time2 = time2;


% storage for draws
          bsave = zeros(ndraw-nomit,k);
          if mm~= 0
          rsave = zeros(ndraw-nomit,1);
          end;
          psave = zeros(ndraw-nomit,1);
          ssave = zeros(ndraw-nomit,1);
          vmean = zeros(n,1);
          acc_rate = zeros(ndraw,1);

ntrs = 101;

% ====== initializations
% compute this stuff once to save time
TI = inv(T);
TIc = TI*c;
iter = 1;

in = ones(n,1);
V = in;
vi = in;
Wy = sparse(W)*y;
x = xsdm;

switch novi_flag
    
case{0} % we do heteroscedastic model    

hwait = waitbar(0,'sdm: MCMC sampling ...');

t0 = clock;                  
iter = 1;
          while (iter <= ndraw); % start sampling;
                  
          % update beta   
          xs = matmul(sqrt(V),x);
          ys = sqrt(V).*y;
          Wys = sqrt(V).*Wy;
          AI = (xs'*xs + sige*TI)\eye(k);         
          yss = ys - rho*Wys;          
          xpy = xs'*yss;
          b = xs'*yss + sige*TIc;
          b0 = (xs'*xs + sige*TI)\b;
          bhat = norm_rnd(sige*AI) + b0;  
          xb = xs*bhat;
                    
          % update sige
          nu1 = n + 2*nu; 
          e = (yss - xb);
          d1 = 2*d0 + e'*e;
          chi = chis_rnd(1,nu1);
          sige = d1/chi;

          % update vi
          ev = y - rho*Wy - x*bhat; 
          chiv = chis_rnd(n,rval+1);  
          %chiv = chi2rnd(rval+1,n,1); % Statistics Toolbox function
          vi = ((ev.*ev/sige) + in*rval)./chiv;
          V = in./vi; 
                        
          % update rval
          if mm ~= 0           
          rval = gamm_rnd(1,1,mm,kk);  
          end;
          
      % we use griddy Gibbs to perform rho-draw
          b0 = (xs'*xs + sige*TI )\(xs'*ys + sige*TIc);
          bd = (xs'*xs + sige*TI)\(xs'*Wys + sige*TIc);
          e0 = ys - xs*b0;
          ed = Wys - xs*bd;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
          rho = draw_rho(detval,epe0,eped,epe0d,n,k,rho,a1,a2);
          
    if iter > nomit % if we are past burn-in, save the draws
        bsave(iter-nomit,1:k) = bhat';
        ssave(iter-nomit,1) = sige;
        psave(iter-nomit,1) = rho;
        vmean = vmean + vi; 
        if mm~= 0
            rsave(iter-nomit,1) = rval;
        end;         
    end;
                    
iter = iter + 1; 
waitbar(iter/ndraw);         
end; % end of sampling loop
close(hwait);

time3 = etime(clock,t0);
results.time3 = time3;

case{1} % we do homoscedastic model 
    
hwait = waitbar(0,'sdm: MCMC sampling ...');

t0 = clock;                  
xpx = x'*x;
xpy = x'*y;
Wy = W*y;
xpWy = x'*Wy;

iter = 1;
          while (iter <= ndraw); % start sampling;
                  
          % update beta   
          AI = (xpx + sige*TI)\eye(k);        
          ys = y - rho*Wy;          
          b = x'*ys + sige*TIc;
          b0 = (xpx + sige*TI)\b;
          bhat = norm_rnd(sige*AI) + b0;  
          xb = x*bhat;
          
          % update sige
          nu1 = n + 2*nu; 
          %e = e0 - rho*ed;
          e = (ys - xb);
          d1 = 2*d0 + e'*e;
          chi = chis_rnd(1,nu1);
          sige = d1/chi;
          
          % update rho using griddy Gibbs
          AI = (xpx + sige*TI)\eye(k);
          b0 = (xpx + sige*TI)\(xpy + sige*TIc);
          bd = (xpx + sige*TI)\(xpWy + sige*TIc);
          e0 = y - x*b0;
          ed = Wy - x*bd;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
          rho = draw_rho(detval,epe0,eped,epe0d,n,k,rho,a1,a2);

    if iter > nomit % if we are past burn-in, save the draws
    bsave(iter-nomit,1:k) = bhat';
    ssave(iter-nomit,1) = sige;
    psave(iter-nomit,1) = rho;
    vmean = vmean + vi; 
    end;
                    
iter = iter + 1; 
waitbar(iter/ndraw);         
end; % end of sampling loop
close(hwait);

time3 = etime(clock,t0);
results.time3 = time3;

otherwise
error('sdm_g: unrecognized prior.novi_flag value on input');
% we should never get here

end; % end of homoscedastic vs. heteroscedastic vs. log-marginal options

% calculate effects estimates
        
t0 = clock; 

% pre-calculate traces for the x-impacts calculations
uiter=50;
maxorderu=100;
nobs = n;
rv=randn(nobs,uiter);
tracew=zeros(maxorderu,1);
wjjju=rv;
for jjj=1:maxorderu
    wjjju=W*wjjju;
    tracew(jjj)=mean(mean(rv.*wjjju));
    
end

traces=[tracew];
traces(1)=0;
traces(2)=sum(sum(W'.*W))/nobs;
trs=[1;traces];
ntrs=length(trs);
trbig=trs';
trbig2 = [trbig(1,2:end) trbig(1,end)];
trmat = [trbig
         trbig2];

                 
        if cflag == 1
        bdraws = bsave(:,2:end);
        elseif cflag == 0
        bdraws = bsave;
        end; 
        pdraws = psave;

        ree = 0:1:ntrs-1;

        rmat = zeros(1,ntrs);
        total = zeros(ndraw-nomit,p,ntrs);
        direct = zeros(ndraw-nomit,p,ntrs);
        indirect = zeros(ndraw-nomit,p,ntrs);
        
for i=1:ndraw-nomit;
    rmat = pdraws(i,1).^ree;
    for j=1:p;
            beta = [bdraws(i,j) bdraws(i,j+p)];
            total(i,j,:) = (beta(1,1) + beta(1,2))*rmat;
    direct(i,j,:) = (beta*trmat).*rmat;
    indirect(i,j,:) = total(i,j,:) - direct(i,j,:);
    end;

end;

time4 = etime(clock,t0);
results.time4 = time4;


% compute posterior means for return arguments
bmean = mean(bsave);
beta = bmean';
rho = mean(psave);
sige = mean(ssave);
results.sige = sige;
vmean = vmean/(ndraw-nomit);
V = in./vmean;

% calculate log-marginal likelihood
[nobs,nvar] = size(x);
          xs = matmul(x,sqrt(V));
          ys = sqrt(V).*y;
          Wys = sqrt(V).*Wy;
          b0 = (xs'*xs + sige*TI)\(xs'*ys + sige*TIc);
          bd = (xs'*xs + sige*TI)\(xs'*Wys + sige*TIc);
          e0 = ys - xs*b0;
          ed = Wys - xs*bd;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
 logdetx = log(det(xs'*xs + sige*TI));
  if inform_flag == 0
   mlike = sdm_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,logdetx,a1,a2);
  elseif inform_flag == 1
   mlike = sdm_marginal2(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,logdetx,a1,a2,c,TI,xs,ys,sige,W);
  end;

 yhat = (speye(nobs) - rho*W)\(xs*beta);
 e = y - yhat;

% compute R-squared
epe = e'*e;
sige = epe/(n-k);
results.sigma = sige;
ym = y - mean(y);
rsqr1 = epe;
rsqr2 = ym'*ym;
results.rsqr = 1- rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared

time = etime(clock,timet);


results.meth  = 'sdm_g';
results.total = total;
results.direct = direct;
results.indirect = indirect;
results.beta = beta;
results.rho = rho;
results.bdraw = bsave;
results.pdraw = psave;
results.sdraw = ssave;
results.beta_std = std(bsave)';
results.sige_std = std(ssave);
results.rho_std = std(psave);
results.mlike = mlike;
results.vmean = vmean;
results.yhat  = yhat;
results.resid = e;
results.bmean = c;
results.bstd  = sqrt(diag(T));
results.ndraw = ndraw;
results.nomit = nomit;
results.time  = time;
results.nu = nu;
results.d0 = d0;
results.a1 = a1;
results.a2 = a2;
results.tflag = 'plevel';
results.rmax = rmax; 
results.rmin = rmin;
results.lflag = ldetflag;
results.lndet = detval;
results.novi  = novi_flag;
results.priorb = inform_flag;
results.cflag = cflag;
results.p = p;

if mm~= 0
results.rdraw = rsave;
results.m     = mm;
results.k     = kk;
else
results.r     = rval;
results.rdraw = 0;
end;


% =========================================================================
% support functions below
% =========================================================================

function rho = draw_rho(detval,epe0,eped,epe0d,n,k,rho,a1,a2)
% PURPOSE: draws rho-values using griddy Gibbs and inversion
% ---------------------------------------------------
%  USAGE: rho = draw_rho(detval,epe0,eped,epe0d,n,k,rho,a1,a2)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------
% REFERENCES: LeSage and Pace (2009) Introduction to Spatial Econometrics
% Chapter 5, pp 136-141 on Bayesian spatial regression models.

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com


nmk = (n-k)/2;
nrho = length(detval(:,1));
iota = ones(nrho,1);

z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
z = -nmk*log(z);
den =  detval(:,2) + z;

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

% To see how this works, uncomment the following lines
% plot(detval(:,1),den/1000,'-');
% line([detval(idraw,1) detval(idraw,1)],[0 den(idraw,1)/1000]);
% hold on;
% line([detval(idraw,1) 0],[den(idraw,1)/1000 den(idraw,1)/1000]);
% drawnow;
% pause;



function [nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,novi_flag,c,T,inform_flag,a1,a2] = sdm_parse(prior,k)
% PURPOSE: parses input arguments for sdm_g models
% ---------------------------------------------------
%  USAGE: [nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,novi_flag,c,T,inform_flag, ...
%          ,a1,a2,logmflag] = sdm_parse(prior,k)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com

% set defaults
eflag = 0;     % default to not computing eigenvalues
ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
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
a1 = 1.0; % default to uniform prior on rho, -1 to 1 interval
a2 = 1.0;
c = zeros(k,1);   % diffuse prior for beta
T = eye(k)*1e+12;
prior_beta = 0;   % flag for diffuse prior on beta
novi_flag = 0; % do vi-estimates
inform_flag = 0;
sflag = 0; % default to sar model

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
    elseif strcmp(fields{i},'a1')
       a1 = prior.a1; 
    elseif strcmp(fields{i},'a2')
       a2 = prior.a2; 
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval   
    elseif strcmp(fields{i},'beta')
        c = prior.beta; inform_flag = 1; % flag for informative prior on beta
    elseif strcmp(fields{i},'bcov')
        T = prior.bcov; inform_flag = 1;
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
        error('sdm_g: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = prior.order;  
    elseif strcmp(fields{i},'iter')
        iter = prior.iter; 
    elseif strcmp(fields{i},'novi')
        novi_flag = prior.novi;
    elseif strcmp(fields{i},'eig')
        eflag = prior.eig;
    end;
 end;

 
else, % the user has input a blank info structure
      % so we use the defaults
end; 


function [rmin,rmax,time2] = sdm_eigs(eflag,W,rmin,rmax,n);
% PURPOSE: compute the eigenvalues for the weight matrix
% ---------------------------------------------------
%  USAGE: [rmin,rmax,time2] = far_eigs(eflag,W,rmin,rmax,W)
% where eflag is an input flag, W is the weight matrix
%       rmin,rmax may be used as default outputs
% and the outputs are either user-inputs or default values
% ---------------------------------------------------


if eflag == 1 % compute eigenvalues
t0 = clock;
opt.tol = 1e-3; opt.disp = 0;
lambda = eigs(sparse(W),speye(n),1,'SR',opt);  
rmin = 1/real(lambda);   
rmax = 1;
time2 = etime(clock,t0);
else
time2 = 0;
end;


function [detval,time1] = sdm_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
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
            error('sdm_g: wrong lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('sdm_g: wrong sized lndet input argument');
        elseif n1 == 1
            error('sdm_g: wrong sized lndet input argument');
        end;          
end;

function  out = sdm_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,logdetx,a1,a2)
% PURPOSE: returns a vector of the log-marginal over a grid of rho-values
% -------------------------------------------------------------------------
% USAGE: out = sdm_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,logdetx,a1,a2)
% where:       detval = an ngrid x 2 matrix with rho-values and lndet values
%                  e0 = y - x*b0;
%                 ed = Wy - x*bd;
%               epe0 = e0'*e0;
%               eped = ed'*ed;
%              epe0d = ed'*e0;
%               nobs = # of observations
%               nvar = # of explanatory variables
%            logdetx = log(det(x'*x))
%                 a1 = parameter for beta prior on rho
%                 a2 = parameter for beta prior on rho
% -------------------------------------------------------------------------
% RETURNS: out = a structure variable
%        out = log marginal, a vector the length of detval
% -------------------------------------------------------------------------
% NOTES: -this does not take any prior on beta, sigma into account, uses diffuse priors
%         see sdm_marginal2() 
% -------------------------------------------------------------------------

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com

n = length(detval);
nmk = (nobs-nvar)/2;
% C is a constant of integration that can vary with nvars, so for model
% comparisions involving different nvars we need to include this
bprior = beta_prior(detval(:,1),a1,a2);
C = log(bprior) + gammaln(nmk) - nmk*log(2*pi) - 0.5*logdetx;
iota = ones(n,1);
z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
den = C + detval(:,2) - nmk*log(z);
out = real(den);

function  out = sdm_marginal2(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,a1,a2,c,TI,xs,ys,sige,W)
% PURPOSE: returns a vector of the log-marginal over a grid of rho-values
%          for the case of an informative prior on beta
% -------------------------------------------------------------------------
% USAGE: out = sdm_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,logdetx,a1,a2)
% where:       detval = an ngrid x 2 matrix with rho-values and lndet values
%               nobs = # of observations
%               nvar = # of explanatory variables
%            logdetx = log(det(x'*x))
%                 a1 = parameter for beta prior on rho
%                 a2 = parameter for beta prior on rho
% -------------------------------------------------------------------------
% RETURNS: out = a structure variable
%        out = log marginal, a vector the length of detval
% -------------------------------------------------------------------------
% NOTES: -this is only an approximation based on the posterior mean Vi-estimates
% -------------------------------------------------------------------------

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com

n = length(detval);
nmk = (nobs-nvar)/2;
% C is a constant of integration that can vary with nvars, so for model
% comparisions involving different nvars we need to include this
bprior = beta_prior(detval(:,1),a1,a2);
C = log(bprior) + gammaln(nmk) - nmk*log(2*pi);
iota = ones(n,1);
z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
% add quadratic terms based on prior for beta
Q1 = zeros(n,1);
Q2 = zeros(n,1);
xpxi = inv(xs'*xs);
sTI = sige*TI;
xpxis = inv(xs'*xs + sTI);
logdetx = log(det(xpxis));
C = C - 0.5*logdetx;
          for i=1:n;
           rho = detval(i,1);
           D = speye(nobs) - rho*W;
           bhat = xpxi*(xs'*D*ys);
           beta = xpxis*(xs'*D*ys + sTI*c); 
           Q1(i,1) = (c - beta)'*sTI*(c - beta);
           Q2(i,1) = (bhat - beta)'*(xs'*xs)*(bhat - beta);
          end;

den = C + detval(:,2) - nmk*log(z + Q1 + Q2);
out = real(den);

