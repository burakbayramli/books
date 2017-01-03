function results=sac_gmm(y,x,W,M,options)
% PURPOSE: computes Generalized Moments Estimates for general spatial model
%           y = rho*W*y + XB + u, u = lam*M*u + e
% ---------------------------------------------------
%  USAGE: results = sac_gmm(y,x,W,M,options)
%  where: y = dependent variable vector
%          x = explanatory variables matrix, (with intercept term in first
%              column if used)
%             (with intercept vector in the 1st column of x)
%         W = sparse contiguity matrix (standardized)
%         M = optional weight matrix (if nargin = 3, M is set = W)
%   options = an optional structure variable with options
%   options.iter = 0 for no iteration (the default)
%                = 1 for iteration
%   options.maxit = maximum # of iterations used during optimization
%                   (default == 1000)
%   options.btol = criterion for GMM parameter convergence
%                   (default = 1e-7)
%   options.ftol = criterion for GMM function convergence
%                   (default = 1e-10)
%   options.prt  = flag for printing of GMM optimization steps
%                = 0 default to not printing
%                = 1 print intermediate results
%   options.ndraw = # of draws for calculating effects estimates (1,000 by default)
% ---------------------------------------------------
%  RETURNS: a structure
%         results.meth       = 'sac_gmm'
%         results.beta       = bhat
%         results.tstat      = asymp t-stats
%         results.rho        = rho
%         results.lam        = lambda
%         results.total    = a 3-d matrix (ndraw,p,ntrs) total x-impacts
%         results.direct   = a 3-d matrix (ndraw,p,ntrs) direct x-impacts
%         results.indirect = a 3-d matrix (ndraw,p,ntrs) indirect x-impacts
%                            ndraw = 2,500 by default, ntrs = 101 default
%                            p = nvar-1 if there is a constant term which we skip
%         results.rhotstat   = t-stat of rho (under normality assumption)
%         results.lamtstat   = t-stat of lam (under normality assumption)
%         results.GMsige     = GM-estimated variance
%         results.yhat       = yhat = A*B*x*bhat, A=inv(I - lam*M), B=inv(I - rho*W)
%         results.resid      = residuals, y - yhat
%         results.sige       = sige = e'*e/n, e = y - yhat
%         results.rsqr       = rsquared
%         results.rbar       = rbar-squared
%         results.se         = Standard errors from EGLS
%         results.nobs       = number of observations
%         results.nvar       = number of variables 
%         results.time1      = time for optimization
%         results.time       = total time taken
%         results.time2      = time for effects estimates calculation
%         results.cflag      = 0 for no intercept term, 1 for intercept term
% ---------------------------------------------------
% %  SEE ALSO: prt(results), sac, sac_g
% ---------------------------------------------------
% REFERENCES: Luc Anselin Spatial Econometrics (1988) pages 182-183.
% Kelejian, H., and  Prucha, I.R.  (1998). A Generalized Spatial Two-Stage
% Least Squares Procedure for Estimating a Spatial Autoregressive
% Model with Autoregressive Disturbances. Journal of Real
% Estate and Finance Economics,  17, 99-121.
% ---------------------------------------------------

% written by: Jim LeSage
% Adapted from Shawn Bucholtz code for the SEM model case

% set defaults
%  arguments for MInZ function;
itermax = 1000;
infoz2.hess='marq';
infoz2.func = 'lsfunc';
infoz2.momt = 'nllsrho_minz';
infoz2.jake = 'numz';%For numerical derivatives
infoz2.call='ls';
infoz2.prt=0;
infoz2.btol=1e-7;
infoz2.ftol=1e-10;
infoz2.maxit=1000;
itflag = 0;

time5 = 0;

ndraw = 1000;
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
error('sac_gmm: intercept term must be in first column of the x-matrix');
elseif length(ind) == 0 % case of no intercept term
cflag = 0;
iflag = 0;
p = size(x,2);
end;
elseif sum(x(:,1)) == n % we have an intercept in the right place
cflag = 1;
iflag = 1;
p = size(x,2)-1;
end;

results.cflag = cflag;
results.p = p;



if nargin == 5 % we need to parse user input options
fields = fieldnames(options);
nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'prt')
        infoz2.prt = options.prt; 
    elseif strcmp(fields{i},'maxit')
        infoz2.maxit = options.maxit;
    elseif strcmp(fields{i},'iter')
        itflag = options.iter;
    elseif strcmp(fields{i},'btol')
        infoz2.btol = options.btol;   
    elseif strcmp(fields{i},'ftol');
        infoz2.ftol = options.ftol;
    elseif strcmp(fields{i},'ndraw');
        ndraw = options.ndraw;
        results.ndraw = ndraw;
  end;
 end;
end;
    


mwflag = 0;
if nargin == 3
    M = W;
    mwflag = 1;
end;

results.meth = 'sac_gmm';


timet = clock; % start the clock for overall timing




% USAGE: results = tsls(y,yendog,xexog,xall)
%   where: y      = dependent variable vector (nobs x 1)
%          yendog = endogenous variables matrix (nobs x g)
%          xexog  = exogenous variables matrix for this equation
%          xall   = all exogenous and lagged endogenous variables 
%                   in the system
%Estimated 2SLS to get a vector of residuals
[n, nvar]=size(x);
results.nobs=n;
results.nvar=nvar;
    if iflag == 1
    Wy = sparse(W)*y;
    Wx = sparse(W)*x(:,2:end);
    z = [x Wx W*Wx];
   o1 = tsls(y,Wy,x,z);
    elseif iflag == 0
    Wy = sparse(W)*y;
    Wx = sparse(W)*x;
    z = [x Wx W*Wx];
    o1 = tsls(y,Wy,x,z);
    end;    

e=o1.resid;  % 1st step residuals

%Make inital guesses at parameter vector;
lambdavec = [.5;o1.sige];

%Begin Interation
econverge = e;
criteria = 0.001;
converge = 1.0;
iter = 0;

t0 = clock;

if itflag ~= 0
while (converge > criteria & iter < itermax)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Input arguments into system of equations and moment
%conditions;
%The notation is simialar to the publication;
%se denotes e with a single dot (W*e);
%de denotes e with a double dot (W*W*e);

% trick for trace(M'*M);
trMM = sum(sum(M.*M));

    se=M*e;
    de=M*se;
    Gn=zeros(3,3);
    Gn(1,1)=(2/n)*e'*se;Gn(1,2)=(-1/n)*se'*se; Gn(1,3)=1;
    Gn(2,1)=(2/n)*se'*de; Gn(2,2)=(-1/n)*de'*de; Gn(2,3)=(1/n)*trMM;
    Gn(3,1)=(1/n)*((e'*de)+(se'*se)); Gn(3,2)=(-1/n)*se'*de; Gn(3,3)=0;
    Gn2=[(1/n)*e'*e;(1/n)*se'*se;(1/n)*e'*se];

    %Pass arguments to MInZ function;
    [lambdahat,infoz2,stat]=minz(lambdavec,infoz2.func,infoz2,Gn,Gn2);

    lambdavec = [lambdahat(1);lambdahat(2)];
    
    %Estimate Parameters using EGLS;
    tmp = speye(n) - lambdahat(1)*sparse(M);

    zs = tmp*z;
    ys = tmp*y;
    Wys = tmp*Wy;
    xs = tmp*x;
    o1 = tsls(ys,Wys,xs,zs);

    e = o1.resid;

    converge = max(abs(e - econverge));%Check convergence
    econverge = e;

    iter = iter + 1;
end;

elseif itflag == 0
    % trick for trace(M'*M);
    trMM = sum(sum(M.*M));

    se=M*e;
    de=M*se;
    Gn=zeros(3,3);
    Gn(1,1)=(2/n)*e'*se;Gn(1,2)=(-1/n)*se'*se; Gn(1,3)=1;
    Gn(2,1)=(2/n)*se'*de; Gn(2,2)=(-1/n)*de'*de; Gn(2,3)=(1/n)*trMM;
    Gn(3,1)=(1/n)*((e'*de)+(se'*se)); Gn(3,2)=(-1/n)*se'*de; Gn(3,3)=0;
    Gn2=[(1/n)*e'*e;(1/n)*se'*se;(1/n)*e'*se];

    %Pass arguments to MInZ function;
    [lambdahat,infoz2,stat]=minz(lambdavec,infoz2.func,infoz2,Gn,Gn2);

    lambdavec = [lambdahat(1);lambdahat(2)];
end

time1 = etime(clock,t0);

results.iter = iter;

%Compute stats from minimization;
e1 = Gn2-Gn*[lambdahat(1);lambdahat(1)^2;lambdahat(2)];
vare1 = std(e1)*std(e1);
se = sqrt(vare1*diag(stat.Hi));
results.lambdatstat=lambdahat(1)./se(1);
results.GMsige=lambdahat(2);

% estimate rho using the lambda-hat estimate
lam = lambdahat(1);
% see Kelejian-Prucha (1998 Journal of Real Estate Finance and Economics)
% page 109 describing the third-step of the procedure
    if iflag == 1
    xs = x(:,2:end) - lam*M*x(:,2:end);
    ys = y - lam*M*y;
    Wys = Wy - lam*W*Wy;
    Wxs = xs - sparse(W)*xs;
    z = [ones(n,1) xs Wxs W*Wxs];
    o1 = tsls(ys,Wys,[ones(n,1) xs],z);
    rho = o1.beta(1,1);
    bhat = o1.beta(2:end,1);
    rhotstat = o1.tstat(1,1);
    btstat = o1.tstat(2:end,1);
    sige = (o1.resid'*o1.resid)/n;
         % construct sigma matrix
     xapxa = inv(z'*z);
xpx = [xs'*xs    xs'*Wys 
       Wys'*xs   Wys'*z*xapxa*z'*Wys];     
sigmat = sige*inv(xpx); 
    elseif iflag == 0
    xs = x - lam*M*x;
    ys = y - lam*M*y;
    Wys = Wy - lam*W*Wy;
    Wxs = x - sparse(W)*x;
    z = [xs Wxs W*Wxs];
    o1 = tsls(ys,Wys,xs,z);
    rho = o1.beta(1,1);
    bhat = o1.beta(2:end,1);
    rhotstat = o1.tstat(1,1);
    btstat = o1.tstat(2:end,1);
    sige = (o1.resid'*o1.resid)/n;
             % construct sigma matrix
     xapxa = inv(z'*z);
xpx = [xs'*xs    xs'*Wys 
       Wys'*xs   Wys'*z*xapxa*z'*Wys];     
sigmat = sige*inv(xpx); 
    end;    

    
% fill-in results structure with EGLS estimates

results.lam =lam;
results.rho = rho;
results.beta = bhat;
results.sige = sige;
results.tstat = btstat;
results.rhotstat = rhotstat;
							
% simulate parameter draws for x-impacts calculations
	t0 = clock;
    
    if iflag == 1
	parm_vec = [results.beta(2:end,1)
				results.rho];
    elseif iflag == 0
   	parm_vec = [results.beta
				results.rho];
    end;     
								
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

        bdraws = draws;
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

results.time1 = time1;
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



