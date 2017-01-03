function results = sdm(y,x,W,info)
% PURPOSE: computes spatial durbin model estimates
%         (I-rho*W)y = a*iota + X*b1 + W*X*b2 + e, using sparse algorithms
% ---------------------------------------------------
%  USAGE: results = sdm(y,x,W,info)
%  where: y = dependent variable vector
%         x = explanatory variables matrix, 
%             (with intercept term in first column if used)
%         W = weight matrix (standardized)
%       info = an (optional) structure variable with input options:
%       info.rmin = (optional) minimum value of rho to use in search  
%       info.rmax = (optional) maximum value of rho to use in search    
%       info.convg = (optional) convergence criterion (default = 1e-8)
%       info.maxit = (optional) maximum # of iterations (default = 500)
%       info.lflag = 0 for full lndet computation (default = 1, fastest)
%                  = 1 for MC lndet approximation (fast for very large problems)
%                  = 2 for Spline lndet approximation (medium speed)
%       info.order = order to use with info.lflag = 1 option (default = 50)
%       info.iter  = iterations to use with info.lflag = 1 option (default = 30) 
%       info.lndet = a matrix returned by sar, sar_g, sarp_g, etc.
%                    containing log-determinant information to save time
%       info.ndraw = 1,000 by default
% ---------------------------------------------------
%  RETURNS: a structure
%         results.meth  = 'sdm'
%         results.beta  = bhat [a b1 b2]' a k+(k-1) x 1 vector if there is a constant term
%                       = bhat [b1 b2]' a 2*k x 1 vector if no constant
%         results.rho   = rho 
%         results.total    = a 3-d matrix (ndraw,p,ntrs) total x-impacts
%         results.direct   = a 3-d matrix (ndraw,p,ntrs) direct x-impacts
%         results.indirect = a 3-d matrix (ndraw,p,ntrs) indirect x-impacts
%                            ndraw = 1,000 by default, ntrs = 101 default
%                            p = k-1 if there is a constant term which we skip
%         results.tstat = t-statistics (last entry is rho)
%         results.yhat  = yhat = inv(I - rho*W)*(a*iota + X*b1 + W*X*b2)
%         results.resid = residuals, y - yhat (from above)
%         results.sige  = sige estimate, e'*e/(n-k)
%         results.rsqr  = rsquared, based on conventional OLS formula
%         results.rbar  = rbar-squared, based on conventional OLS formula
%         results.lik   = log likelihood
%         results.nobs  = nobs
%         results.nvar  = nvars the number of explanatory variables in [iota x W*x] (including intercept) 
%         results.p     = # of explanatory variables in x-matrix excluding the constant term
%         results.cflag = 0 for no intercept term, 1 for intercept term
%         results.y     = y data vector
%         results.iter  = # of iterations taken
%         results.rmax  = 1/max eigenvalue of W (or rmax if input)
%         results.rmin  = 1/min eigenvalue of W (or rmin if input)
%         results.lflag = lflag from input
%         results.miter = info.iter option from input
%         results.order = info.order option from input
%         results.limit = matrix of [rho lower95,logdet approx, upper95] intervals
%                         for the case of lflag = 1
%         results.time1 = time for log determinant calcluation
%         results.time2 = time for eigenvalue calculation
%         results.time3 = time for hessian or information matrix calculation
%         results.time4 = time for optimization
%         results.time5      = time for effects estimates calculation
%         results.time  = total time taken       
%         results.lndet = a matrix containing log-determinant information
%                          (for use in later function calls to save time)
%  --------------------------------------------------
%  SEE ALSO: sdm_d, sdm_d2 demos
% ---------------------------------------------------
%  NOTES: constant term should be in 1st column of the x-matrix if used
%  if you use lflag = 1 or 2, info.rmin will be set = -1 
%                             info.rmax will be set = 1
% ---------------------------------------------------
% REFERENCES: LeSage and Pace (2009) Chapter 4 on maximum likelihood estimation 
%             of spatial regression models.
% For lndet information see: Chapter 4
% For interpretation of direct, indirect and total x-impacts see: Chapter 2
% ---------------------------------------------------

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com


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

[nobs,nvar] = size(xsdm);
info.p = p;
info.cflag = cflag;


time1 = 0; 
time2 = 0;
time3 = 0;
time4 = 0;
time5 = 0;

timet = clock; % start the clock for overall timing

% check size of user inputs for comformability
[n1, n2] = size(W);
if n1 ~= n2
    error('sdm: wrong size weight matrix W');
    elseif n1 ~= n
    error('sdm: wrong size weight matrix W');
end;

% if we have no options, invoke defaults
if nargin == 3
    info.lflag = 1;
end;


% parse input options
[rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,miter,options,ndraw] = sdm_parse(info);


results.ndraw = ndraw;
    
% check if the user handled the intercept term okay
    n = length(y);
    if sum(x(:,1)) ~= n
    tst = sum(x); % we may have no intercept term
    ind = find(tst == n); % we do have an intercept term
         if length(ind) > 0
         error('sdm: intercept term must be in first column of the x-matrix');
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
    

% compute eigenvalues or limits
[rmin,rmax,time2] = sdm_eigs(eflag,W,rmin,rmax,n);
results.time2 = time2;

% do log-det calculations
[detval,time1] = sdm_lndet(ldetflag,W,rmin,rmax,detval,order,miter);
results.time1 = time1;

x = xsdm;

t0 = clock;
          Wy = sparse(W)*y;
          b0 = (x'*x)\(x'*y);
          bd = (x'*x)\(x'*Wy);
          e0 = y - x*b0;
          ed = Wy - x*bd;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;

% step 1) do regressions
% step 2) maximize concentrated likelihood function;
    options = optimset('fminbnd');
    
    [prho,liktmp,exitflag,output] = fminbnd('f_sdm',rmin,rmax,options,detval,epe0,eped,epe0d,n);
   
time4 = etime(clock,t0);
results.time4 = time4;

if exitflag == 0
fprintf(1,'\n sdm: convergence not obtained in %4d iterations \n',output.iterations);
end;
results.iter = output.iterations;

% step 3) find b,sige maximum likelihood estimates
results.beta = b0 - prho*bd; 
results.rho = prho; 
bhat = results.beta;
results.sige = (1/n)*(e0-prho*ed)'*(e0-prho*ed); 
sige = results.sige;

e = (e0 - prho*ed);
yhat = (speye(n) - prho*W)\(x*bhat);
results.yhat = yhat;
results.resid = y - yhat;

parm = [results.rho
        results.beta
        results.sige];

results.lik = f2_sdm(parm,y,x,W,detval);

if n <= 500
t0 = clock;
% asymptotic t-stats based on information matrix
% (page 80-81 Anselin, 1980)
B = eye(n) - prho*W; 
BI = inv(B); WB = W*BI;
pterm = trace(WB*WB + WB*WB');
xpx = zeros(nvar+2,nvar+2);               % bhat,bhat
xpx(1:nvar,1:nvar) = (1/sige)*(x'*x);     % bhat,rho
xpx(1:nvar,nvar+1) = (1/sige)*x'*W*BI*x*bhat;
xpx(nvar+1,1:nvar) = xpx(1:nvar,nvar+1)'; % rho,rho
xpx(nvar+1,nvar+1) = (1/sige)*bhat'*x'*BI'*W'*W*BI*x*bhat + pterm;
xpx(nvar+2,nvar+2) = n/(2*sige*sige);     %sige,sige
xpx(nvar+1,nvar+2) = (1/sige)*trace(WB);  % rho,sige
xpx(nvar+2,nvar+1) = xpx(nvar+1,nvar+2);
hessi = invpd(xpx);

tmp = diag(abs(hessi(1:nvar+1,1:nvar+1)));
bvec = [results.beta
results.rho];
tmps = bvec./(sqrt(tmp));
results.tstat = tmps;
results.bstd = sqrt(tmp(1:nvar,1));
results.pstd = sqrt(tmp(nvar+1,1));
time3 = etime(clock,t0);
results.time3 = time3;
				
else % asymptotic t-stats using numerical hessian

t0 = clock;
parm = [results.beta
        results.rho
        results.sige];
    
dhessn = hessian('f2_sdm',parm,y,x,W,detval);
hessi = -invpd(dhessn);

if any(diag(hessi) < 0)
    fprintf(1,'sdm: negative variances from numerical hessian \n');
    fprintf(1,'sdm: t-statistics may be inaccurate \n');
end;

tvar = abs(diag(hessi));

tmp = [results.beta
       results.rho];
results.tstat = tmp./sqrt(tvar(1:end-1,1));
results.bstd = sqrt(tvar(1:end-2,1));
results.pstd = sqrt(tvar(end-1,1));
time3 = etime(clock,t0);
results.time3 = time3;
							   
end;
							   

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

% cheat here to fix the numerical hessian if it sucks
% Use MCMC to get good results
sigmat = hessi - diag(diag(hessi)) + diag(diag(abs(hessi)));
sigmatt = sigmat(1:end-1,1:end-1);
[R,posdef] = chol(sigmatt);

if posdef ~= 0 % even cheating did not work, so punt with a kludge
    tmp = [x W*y]'*[x W*y];
    sigmatt = sige*(inv(tmp));
end;

tmp = [results.beta
       results.rho];
   
bdraws = matadd(norm_rndmat(sigmatt,ndraw),tmp);
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
            beta = [bdraws(i,j) bdraws(i,j+p)];
            total(i,j,:) = (beta(1,1) + beta(1,2))*rmat;
    direct(i,j,:) = (beta*trmat).*rmat;
    indirect(i,j,:) = total(i,j,:) - direct(i,j,:);
    end;

end;

time5 = etime(clock,t0);
results.time5 = time5;


% ==================================================

ym = y - mean(y);       % r-squared, rbar-squared
rsqr1 = results.resid'*results.resid;
rsqr2 = ym'*ym;
results.rsqr = 1.0-rsqr1/rsqr2;   % r-squared
rsqr1 = rsqr1/(n-nvar);
rsqr2 = rsqr2/(n-1.0);

% return stuff
results.meth = 'sdm';
results.y = y;      
results.total = total;
results.direct = direct;
results.indirect = indirect;
results.nobs = n; 
results.nvar = nvar;
results.rmax = rmax;      
results.rmin = rmin;
results.lflag = ldetflag;
results.order = order;
results.miter = miter;
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
results.time = etime(clock,timet);
results.lndet = detval;




% =========================================================================
% Support functions are here
% =========================================================================




function [rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,iter,options,ndraw] = sdm_parse(info)
% PURPOSE: parses input arguments for sar model
% ---------------------------------------------------
%  USAGE:[rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,iter,options,ndraw,p] = sdm_parse(info)
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
options = zeros(1,18); % optimization options for fminbnd
options(1) = 0; 
options(2) = 1.e-6; 
options(14) = 500;

eflag = 0;     % default to not computing eigenvalues
ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
order = 50;    % there are parameters used by the MC det approx
iter = 30;     % defaults based on Pace and Barry recommendation
rmin = -1;     % use -1,1 rho interval as default
rmax = 1;
detval = 0;    % just a flag
convg = 0.0001;
maxit = 500;
ndraw = 1000;

fields = fieldnames(info);
nf = length(fields);
if nf > 0
    
 for i=1:nf
    if strcmp(fields{i},'rmin')
        rmin = info.rmin;  eflag = 0;
    elseif strcmp(fields{i},'rmax')
        rmax = info.rmax; eflag = 0;
    elseif strcmp(fields{i},'convg')
        options(2) = info.convg;
    elseif strcmp(fields{i},'maxit')
        options(14) = info.maxit;  
    elseif strcmp(fields{i},'lndet')
    detval = info.lndet;
    ldetflag = -1;
    eflag = 0;
    rmin = detval(1,1);
    nr = length(detval);
    rmax = detval(nr,1);
    elseif strcmp(fields{i},'lflag')
        tst = info.lflag;
        if tst == 0,
        ldetflag = 0; % compute full lndet, no approximation
        elseif tst == 1,
        ldetflag = 1; % use Pace-Barry approximation
        elseif tst == 2,
        ldetflag = 2; % use spline interpolation approximation
        else
        error('sdm: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = info.order;  
    elseif strcmp(fields{i},'eig')
        eflag = info.eig;  
    elseif strcmp(fields{i},'iter')
        iter = info.iter; 
     elseif strcmp(fields{i},'ndraw')
        ndraw = info.ndraw; 
     elseif strcmp(fields{i},'sflag')
        sflag = info.sflag; 
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

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com


if eflag == 1 % do eigenvalue calculations
t0 = clock;
opt.tol = 1e-3; opt.disp = 0;
lambda = eigs(sparse(W),speye(n),1,'SR',opt);  
rmin = real(1/lambda);   
rmax = 1.0;
time2 = etime(clock,t0);
else % use rmin,rmax arguments from input or defaults -1,1
time2 = 0;
end;




function [detval,time1] = sdm_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
% PURPOSE: compute the log determinant |I_n - rho*W|
% using the user-selected (or default) method
% ---------------------------------------------------
%  USAGE: detval = sdm_lndet(ldetflag,W,rmin,rmax,detval,order,iter)
% where eflag,rmin,rmax,W contains input flags 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com


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
            error('sdm: wrong lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('sdm: wrong sized lndet input argument');
        elseif n1 == 1
            error('sdm: wrong sized lndet input argument');
        end;          
end;



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
% by 
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
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

