function results = sac(y,x,W1,W2,info)
% PURPOSE: computes general Spatial Model estimates
%  model: y = rho*W1*y + X*b + u,  u = lam*W2*u + e
% ---------------------------------------------------
%  USAGE: results = sac(y,x,W1,W2,info)
%  where: y  = dependent variable vector
%          x = explanatory variables matrix, (with intercept term in first
%              column if used)
%         W1 = spatial weight matrix (standardized)
%         W2 = spatial weight matrix 
%      info        = an (optional) structure variable with input options
%      info.parm   = (optional) 2x1 vector of starting values for rho, lambda
%      info.convg  = (optional) convergence criterion (default = 1e-4)
%      info.maxit  = (optional) maximum # of iterations (default = 500)
%      info.lmin   = (optional) minimum lambda to search (default = -0.99)
%      info.lmax   = (optional) maximum lambda to search (default = 0.99)
%      info.rmin   = (optional) minimum rho to search (default = -0.99)
%      info.rmax   = (optional) maximum rho to search (default = 0.99)
%      info.lflag  = 0 for full computation (default = 1, fastest)
%                  = 1 for Pace and Barry 1999 MC approximation (fast for very large problems)
%                  = 2 for Pace and Barry 1998 Spline approximation (medium speed)
%      info.order  = order to use with info.lflag = 1 option (default = 50)
%      info.iter   = iterations to use with info.lflag = 1 option (default = 30)     
%      info.hessian = 1 for numerical hessian calculation of var-cov matrix
%                     default = 0 if n < 500, analytical hessian
%                             = 1 if n > 500, numerical hessian
%      info.ndraw = 1,000 by default
% ---------------------------------------------------
%  RETURNS: a structure 
%         results.meth  = 'sac'
%         results.beta  = bhat
%         results.rho   = rho
%         results.lam   = lambda
%         results.tstat = asymptotic t-stats (last 2 are rho,lambda)
%         results.yhat  = yhat
%         results.resid = residuals
%         results.sige  = sige = e'(I-L*W)'*(I-L*W)*e/n
%         results.rsqr  = rsquared
%         results.rbar  = rbar-squared
%         results.lik   = likelihood function value
%         results.nobs  = nobs
%         results.nvar  = nvars
%         results.y     = y data vector
%         results.iter  = # of iterations taken
%         results.lflag = lflag from input
%         results.cflag = 0 for no intercept term, 1 for intercept term
%         results.liter = info.iter option from input
%         results.order = info.order option from input
%         results.limit = matrix of [rho lower95,logdet approx, upper95] intervals
%                         for the case of lflag = 1
%         results.time1 = time for log determinant calcluation
%         results.time2 = time for eigenvalue calculation
%         results.time3 = time for hessian or information matrix calculation
%         results.time4 = time for optimization
%         results.cflag = 0 for no intercept term, 1 for intercept term
%         results.p     = # of non-constant explanatory variables
%  --------------------------------------------------
%  SEE ALSO: prt_spat(results), prt
% ---------------------------------------------------
% REFERENCES: Luc Anselin Spatial Econometrics (1988) 
%             pages 64-65 and pages 182-183.
% For lndet information see: Ronald Barry and R. Kelley Pace, 
% "A Monte Carlo Estimator of the Log Determinant of Large Sparse Matrices", 
% Linear Algebra and its Applications", Volume 289, Number 1-3, 1999, pp. 41-54.
% and: R. Kelley Pace and Ronald P. Barry "Simulating Mixed Regressive
% Spatially autoregressive Estimators", 
% Computational Statistics, 1998, Vol. 13, pp. 397-418.
% ---------------------------------------------------

% written by:
% James P. LeSage, 1/2000
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial.econometrics.com

% NOTE: much of the speed for large problems comes from:
% the use of methods pioneered by Pace and Barry.
% R. Kelley Pace was kind enough to provide functions
% lndetmc, and lndetint from his spatial statistics toolbox
% for which I'm very grateful.


timet = clock; % start the clock for overall timing

ndraw = 1000;
results.ndraw = 1000;

rflag = 0;
ldetflag = 1; % default to the fastest method
rflag = 0;
order = 50; liter = 30; % defaults

results.order = order;
results.liter = liter;
hess_flag = 0;
lmin = -0.99;
lmax = 0.99;
rmin = -0.99;
rmax = 0.99;
    parm = [0.5
         0.5];
     
     % check if the user handled the intercept term okay
    n = length(y);
    if sum(x(:,1)) ~= n
    tst = sum(x); % we may have no intercept term
    ind = find(tst == n); % we do have an intercept term
     if length(ind) > 0
     error('sac: intercept term must be in first column of the x-matrix');
     elseif length(ind) == 0 % case of no intercept term
     cflag = 0;
     p = size(x,2);
     end;
    elseif sum(x(:,1)) == n % we have an intercept in the right place
     cflag = 1;
     p = size(x,2)-1;
    end;
     
    results.cflag = cflag;
    results.p = p;
    

% default options
options = optimset('fminsearch');

if nargin == 5
 if ~isstruct(info)
 error('sac: must supply the options as a structure variable');
 end;
options.MaxIter = 500;
 fields = fieldnames(info);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'parm')
       parm = info.parm;
    elseif strcmp(fields{i},'convg')
       options.TolFun = info.convg;
    elseif strcmp(fields{i},'maxit')
        options.MaxIter  = info.maxit;
    elseif strcmp(fields{i},'rmin')
        rmin = info.rmin;
    elseif strcmp(fields{i},'rmax')
        rmax = info.rmax;
    elseif strcmp(fields{i},'lmin')
        lmin = info.lmin;
    elseif strcmp(fields{i},'lmax')
        lmax = info.lmax;
    elseif strcmp(fields{i},'hessian')
        hess_flag  = info.hessian;
    elseif strcmp(fields{i},'lflag')
        ldetflag = info.lflag;
    elseif strcmp(fields{i},'order')
        order = info.order;  results.order = order;
    elseif strcmp(fields{i},'iter')
    liter = info.iter; results.liter = liter;
    elseif strcmp(fields{i},'ndraw')
    ndraw = info.ndraw; results.ndraw = ndraw;
    end;
 end;
elseif nargin == 4 % use default options
options = optimset('fminsearch');
else
 error('Wrong # of arguments to sac'); 
end; 


[n nvar] = size(x);
results.meth = 'sac';

[n1 n2] = size(W1);
if n1 ~= n2
error('sac: wrong size weight matrix W1');
elseif n1 ~= n
error('sac: wrong size weight matrix W1');
end;

[n1 n2] = size(W2);
if n1 ~= n2
error('sac: wrong size weight matrix W2');
elseif n1 ~= n
error('sac: wrong size weight matrix W2');
end;

results.y = y;      
results.nobs = n; results.nvar = nvar;
results.meth = 'sac';

% do lndet approximation calculations if needed
if ldetflag == 0 % no approximation
t0 = clock;    
out = lndetfull(W1,rmin,rmax);
time1 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det1 = [tt' outi];

t0 = clock;    
out = lndetfull(W2,lmin,lmax);
time1 = time1 + etime(clock,t0);
results.time1 = time1;

tt=lmin:.001:lmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det2 = [tt' outi];

elseif ldetflag == 1 % use Pace and Barry, 1999 MC approximation

t0 = clock;    
out = lndetmc(order,liter,W1,rmin,rmax);
time1 = etime(clock,t0);
results.limit = [out.rho out.lo95 out.lndet out.up95];
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det1 = [tt' outi];

t0 = clock;    
out = lndetmc(order,liter,W2,lmin,lmax);
time1 = time1 + etime(clock,t0);
results.time1 = time1;

results.limit = [out.rho out.lo95 out.lndet out.up95];
tt=lmin:.001:lmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det2 = [tt' outi];

elseif ldetflag == 2 % use Pace and Barry, 1998 spline interpolation

t0 = clock;
out = lndetint(W1);
tt=.001:.001:1; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det1 = [tt' outi];

t0 = clock;
out = lndetint(W2);
time1 = time1 + etime(clock,t0);
results.time1 = time1;
tt=.001:.001:1; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det2 = [tt' outi];

end;

% find good starting values
% using Kelejian and Prucha GMM estimation
res0 = sac_gmm(y,x,W1,W2);
parm = [res0.rho
        res0.lam];

timeo = clock;
[pout,like,exitflag,output]=fminsearch('f_sac',parm,options,y,x,W1,W2,det1,det2);
time4 = etime(clock,timeo);
results.time4 = time4;

if exitflag == 0 
fprintf(1,'\n sac: convergence not obtained in %4d iterations \n',output.iterations);
end;
results.iter = output.iterations;


rho = pout(1,1);
lam = pout(2,1);


% fill-in results


A = speye(n) - rho*sparse(W1);
B = speye(n) - lam*sparse(W2);
Bx = (speye(n) - lam*W2)*x;
b = (Bx'*Bx)\(Bx'*B*A*y);
e = B*(A*y - x*b);
results.beta = b;
results.rho = rho;
results.lam = lam;
results.resid = e;
results.yhat = A\Bx;
sigu = e'*e;
sige = sigu/n;
results.sige = sige;



if (hess_flag == 0 & n <= 500)
    
t0 = clock;

% find asymptotic t-stats (from Anselin, 1982, pages 183-184
bhat = results.beta;
xpx = zeros(nvar+3,nvar+3);
BI = inv(B); AI = inv(A); WB = W2*BI; WA = W1*AI;
omeg = sige*eye(n); omegi = (1/sige)*eye(n);
% t-stats for beta
xpx(1:nvar,1:nvar) = (1/sige)*(x'*B'*B*x);
% t-stats for rho
%term1 = trace(WA.*WA);
term1 = trace(WA*WA);
term2 = trace(omeg*(B*WA*BI)'*omegi*(B*WA*BI));
term3 = (B*WA*x*bhat)'*omegi*(B*WA*x*bhat);
xpx(nvar+1,nvar+1) = term1+term2+term3;
% t-stats for lam
%term1 = trace(WB.*WB);
term1 = trace(WB*WB);
%term2 = (1/sige)*(W2*(A*y-x*bhat))'*(W2*(A*y-x*bhat));
term2 = trace(omeg*WB'*omegi*WB);
xpx(nvar+2,nvar+2) = term1+term2;
% sige,sige
xpx(nvar+3,nvar+3) = n/(2*sige*sige);
% off-diagonal terms bhat x rho
xpx(1:nvar,nvar+1) = (1/sige)*(x'*B'*B*WA*x*bhat);
xpx(nvar+1,1:nvar) = xpx(1:nvar,nvar+1)';
% off-diagonal terms bhat x lam
xpx(nvar+2,1:nvar) = zeros(1,nvar);
xpx(1:nvar,nvar+2) = xpx(nvar+2,1:nvar)';
% beta,sige = 0
% beta,rho = 0
% sige,rho
xpx(nvar+3,nvar+1) = (1/sige)*trace(W1*AI);
xpx(nvar+1,nvar+3) = xpx(nvar+3,nvar+1);
% sige,lambda
xpx(nvar+3,nvar+2) = (1/sige)*trace(W2*BI);
xpx(nvar+2,nvar+3) = xpx(nvar+3,nvar+2);
% off-diagonal terms rho x lam
% off-diagonal terms rho x lam
term1 = trace((WB)'*omegi*B*WA*BI*omeg);
term2 = trace(W2*WA*BI);
xpx(nvar+1,nvar+2) = term1+term2;
xpx(nvar+2,nvar+1) = xpx(nvar+1,nvar+2);
hessi = invpd(xpx);
tmp = diag(abs(hessi));
bvec = [results.beta
        results.rho
        results.lam];
results.tstat = bvec./sqrt(tmp(1:nvar+2,1));

std_rhoi = results.tstat(end-1,1)/results.rho;


time3 = etime(clock,t0);
results.time3 = time3;


elseif (n > 500 | hess_flag == 1) % use numerical hessian
t0 = clock;

parm = [results.beta
        results.rho
        results.lam
        results.sige];

if ldetflag == 0
hessn = hessian('f2_sac',parm,y,x,W1,W2,det1,det2);
else
hessn = hessian('f2_sac',parm,y,x,W1,W2,det1,det2);
end;

hessi = invpd(-hessn);

if any(diag(hessi) < 0)
    fprintf(1,'sac: negative variances from numerical hessian \n');
    fprintf(1,'sac: t-statistics may be inaccurate \n');
end;

tmpx = abs(diag(hessi(1:nvar+2,1:nvar+2)));
tmp = [results.beta
       results.rho
       results.lam];
results.tstat = tmp./sqrt(tmpx);
std_rhoi = results.tstat(end-1,1)/results.rho;

time3 = etime(clock,t0);
results.time3 = time3;

end;    

parm = [results.beta
        results.rho
        results.lam
        results.sige];


results.lik = f2_sac(parm,y,x,W1,W2,det1,det2);

% simulate parameter draws for x-impacts calculations

t0 = clock;

% pre-calculate traces for the x-impacts calculations
uiter=50;
maxorderu=100;
nobs = n;
rv=randn(nobs,uiter);
tracew=zeros(maxorderu,1);
wjjju=rv;
for jjj=1:maxorderu
    wjjju=W1*wjjju;
    tracew(jjj)=mean(mean(rv.*wjjju));
    
end

traces=[tracew];
traces(1)=0;
traces(2)=sum(sum(W1'.*W1))/nobs;
trs=[1;traces];
ntrs=length(trs);
trbig=trs';

% cheat here to fix the numerical hessian if it sucks
% Use MCMC to get good results
sigmat = hessi - diag(diag(hessi)) + diag(diag(abs(hessi)));
sigmatt = sigmat(1:end-2,1:end-2);
[R,posdef] = chol(sigmatt);

if posdef ~= 0 % even cheating did not work, so punt with a kludge
    tmp = [x W1*y]'*[x W1*y];
    sigmatt = sige*(inv(tmp));
end;


% simulate parameter draws for x-impacts calculations
parm_vec = [results.beta
            results.rho];

bdraws = matadd(norm_rndmat(sigmatt,ndraw),parm_vec);
draws = bdraws';

psave = draws(:,end);
ind = find(psave > 1); % find bad rho draws
psave(ind,1) = 0.99; % replace them with 0.99


bsave = draws(:,1:end-1);

        if cflag == 1
        bdraws = bsave(:,2:end);
        elseif cflag == 0
        bdraws = bsave;
        end; 
        pdraws = psave;
        
        ree = 0:1:ntrs-1;

        rmat = zeros(1,ntrs);
        total = zeros(ndraw,p,ntrs);
        direct = zeros(ndraw,p,ntrs);
        indirect = zeros(ndraw,p,ntrs);

        
for i=1:ndraw;
    rmat = pdraws(i,1).^ree;
    for j=1:p;
            beta = [bdraws(i,j)];
            total(i,j,:) = beta(1,1)*rmat;
    direct(i,j,:) = (beta*trbig).*rmat;
    indirect(i,j,:) = total(i,j,:) - direct(i,j,:);
    end;

end;

time5 = etime(clock,t0);
results.time5 = time5;


    
% r-squared and rbar-squared
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(n-nvar);
rsqr2 = rsqr2/(n-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
results.time = etime(clock,timet);
results.time1 = time1;
results.total = total;
results.direct = direct;
results.indirect = indirect;



function y = norm_rndmat(sig,ndraw)
% PURPOSE: random multivariate random vector based on
%          var-cov matrix sig
%---------------------------------------------------
% USAGE:   y = norm_rnd(sig)
% where:   sig = a square-symmetric covariance matrix 
% NOTE: for mean b, var-cov sig use: b +  norm_rnd(sig) 
%---------------------------------------------------      
% RETURNS: y = random vector normal draw mean 0, var-cov(sig)
%---------------------------------------------------

% by 
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com

if nargin ~= 2
error('Wrong # of arguments to norm_rnd');
end;

h = chol(sig);
[nrow, ncol] = size(sig);
rv = randn(nrow,ndraw);

y = h'*rv;




function H = hessian(f,x,varargin)
% PURPOSE: Computes finite difference Hessian
% -------------------------------------------------------
% Usage:  H = hessian(func,x,varargin)
% Where: func = function name, fval = func(x,varargin)
%           x = vector of parameters (n x 1)
%    varargin = optional arguments passed to the function
% -------------------------------------------------------
% RETURNS:
%           H = finite differnce hessian
% -------------------------------------------------------

% Code from:
% COMPECON toolbox [www4.ncsu.edu/~pfackler]
% documentation modified to fit the format of the Ecoometrics Toolbox
% by James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

eps = 1e-8;

n = size(x,1);
fx = feval(f,x,varargin{:});
 
% Compute the stepsize (h)
h = eps.^(1/3)*max(abs(x),1e-2);
xh = x+h;
h = xh-x;    
ee = sparse(1:n,1:n,h,n,n);
 
% Compute forward step 
g = zeros(n,1);
for i=1:n
  g(i) = feval(f,x+ee(:,i),varargin{:});
end
   
H=h*h';
% Compute "double" forward step 
for i=1:n
for j=i:n
  H(i,j) = (feval(f,x+ee(:,i)+ee(:,j),varargin{:})-g(i)-g(j)+fx)/H(i,j);
  H(j,i) = H(i,j);
end
end

