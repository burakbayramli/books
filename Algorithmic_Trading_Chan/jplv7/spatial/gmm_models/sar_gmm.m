function results=sar_gmm(y,x,W,info)
% PURPOSE: computes Generalized Moments Estimates for Spatial Autoregressive Model
%           y = rho*W*y + XB + e
% ---------------------------------------------------
%  USAGE: results = sar_gmm(y,x,W,info)
%  where:  y = dependent variable vector
%          x = independent variables matrix
%              (with intercept vector in the 1st column of x)
%          W = sparse contiguity matrix (standardized)
% info.ndraw = # of draws for calculating effects estimates (1,000 by default)
% ---------------------------------------------------
%  RETURNS: a structure
%         results.meth       = 'sar_gmm'
%         results.beta       = bhat
%         results.tstat      = asymp t-stats
%         results.rho        = rho
%         results.total    = a 3-d matrix (ndraw,p,ntrs) total x-impacts
%         results.direct   = a 3-d matrix (ndraw,p,ntrs) direct x-impacts
%         results.indirect = a 3-d matrix (ndraw,p,ntrs) indirect x-impacts
%                            ndraw = 1,000 by default, ntrs = 101 default
%                            p = nvar-1 if there is a constant term which we skip
%         results.rhotstat   = t-stat of rho (under normality assumption)
%         results.GMsige     = GM-estimated variance
%         results.yhat       = yhat = B*x*bhat, B=inv(I - rho*W)
%         results.resid      = residuals, y - yhat
%         results.sige       = sige = e'*e/n, e = y - yhat
%         results.rsqr       = rsquared
%         results.rbar       = rbar-squared
%         results.se         = Standard errors from EGLS
%         results.nobs       = number of observations
%         results.nvar       = number of variables 
%         results.time       = total time taken
%         results.time1      = time for effects estimates calculation
% ---------------------------------------------------
% %  SEE ALSO: prt(results), sar, sar_g
% ---------------------------------------------------
% REFERENCES: Luc Anselin Spatial Econometrics (1988) pages 182-183.
% Kelejian, H., and  Prucha, I.R.  (1998). A Generalized Spatial Two-Stage
% Least Squares Procedure for Estimating a Spatial Autoregressive
% Model with Autoregressive Disturbances. Journal of Real
% Estate and Finance Economics,  17, 99-121.
% ---------------------------------------------------

% Adapted from Shawn Bucholtz code for the SEM model case
% written by:
% James P. LeSage, last revised 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com



timet = clock; % start the clock for overall timing
time5 = 0;

if nargin == 3
    ndraw = 1000;
elseif nargin == 4
    ndraw = info.ndraw;
end;

results.ndraw = ndraw;

% error checking on inputs
xsum = sum(x);
[n,k] = size(x);
ind = find(xsum == n);
iflag = 0;
if length(ind) > 0 % we have an intercept
    if ind ~= 1
    warning('intercept must be in 1st column of the x-matrix');
    end;
    iflag = 1;
end;

% check if the user handled the intercept term okay
    n = length(y);
    if sum(x(:,1)) ~= n
    tst = sum(x); % we may have no intercept term
    ind = find(tst == n); % we do have an intercept term
     if length(ind) > 0
     error('sar: intercept term must be in first column of the x-matrix');
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

results.meth = 'sar_gmm';



%Estimated OLS to get a vector of residuals
[n, nvar]=size(x);
results.nobs=n;
results.nvar=nvar;
    if iflag == 1
       Wy = sparse(W)*y;
       Wx = sparse(W)*x(:,2:end);
        z = [x Wx W*Wx];
       o1 = tsls(y,Wy,x,z);
      rho = o1.beta(1,1);
     bhat = o1.beta(2:end,1);
 rhotstat = o1.tstat(1,1);
   btstat = o1.tstat(2:end,1);
     sige = (o1.resid'*o1.resid)/n;
     % construct sigma matrix
     xapxa = inv(z'*z);
xpx = [x'*x    x'*Wy 
       Wy'*x   Wy'*z*xapxa*z'*Wy];     
sigmat = sige*inv(xpx); 
    elseif iflag == 0
    Wy = sparse(W)*y;
    Wx = sparse(W)*x;
    z = [x Wx W*Wx];
    o1 = tsls(y,Wy,x,z);
    rho = o1.beta(1,1);
    bhat = o1.beta(2:end,1);
    rhotstat = o1.tstat(1,1);
    btstat = o1.tstat(2:end,1);
    sige = (o1.resid'*o1.resid)/n;
    % construct sigma matrix
     xapxa = inv(z'*z);
xpx = [x'*x    x'*Wy 
       Wy'*x   Wy'*z*xapxa*z'*Wy];     
sigmat = sige*inv(xpx); 
    end;    
    
results.rho = rho;
results.beta = bhat;
results.sige = sige;
results.tstat = btstat;
results.rhotstat = rhotstat;

% simulate parameter draws for x-impacts calculations
parm_vec = [results.beta
            results.rho];
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


bdraws = matadd(norm_rndmat(sigmat,ndraw),parm_vec);
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

time1 = etime(clock,t0);
results.time1 = time1;

results.yhat = (speye(n) - rho*W)\x*bhat;

sigu = results.sige*n;
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(n-nvar);
rsqr2 = rsqr2/(n-1.0);
if rsqr2 ~= 0
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
else
    results.rbar = results.rsqr;
end;

time2 = etime(clock,timet);

results.time = time2;

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



