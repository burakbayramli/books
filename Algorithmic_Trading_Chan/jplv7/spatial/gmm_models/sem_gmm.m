function results=sem_gmm(y,x,W,options)
% PURPOSE: computes GMM Estimates for Spatial Error Model
%           using 1 weight matrix
%           y = XB + u,  u = lambda*W*u +e, using sparse algorithms
% ---------------------------------------------------
%  USAGE: results = sem_gmm(y,x,W,options)
%  where: y = dependent variable vector
%         x = independent variables matrix
%             (with intercept vector in the 1st column of x)
%         W = sparse contiguity matrix (standardized)
%   options = an optional structure variable with options
%   options.iter = 0 for no EGLS iteration (the default)
%                = 1 for EGLS iteration
%   options.maxit = maximum # of iterations used during GMM optimization
%                   (default == 1000)
%   options.btol = criterion for GMM parameter convergence
%                   (default = 1e-7)
%   options.ftol = criterion for GMM function convergence
%                   (default = 1e-10)
%   options.prt  = flag for printing of GMM optimization steps
%                = 0 default to not printing
%                = 1 print intermediate results
% ---------------------------------------------------
%  RETURNS: a structure
%         results.meth          = 'sem_gmm'
%         results.beta          = bhat
%         results.tstat         = asymp t-stats from EGLS
%         results.lambda        = lambda
%         results.lambdatstat   = t-stat of lambda (under normality assumption)
%         results.GMsige        = GM-estimated variance
%         results.sige          = Kelejian-Prucha sige estimate, e'(I-p*W)'*(I-p*W)*e/n
%         results.yhat          = yhat = x*bhat
%         results.resid         = residuals, y - yhat
%         results.rsqr          = rsquared
%         results.rbar          = rbar-squared
%         results.se            = Standard errors from EGLS
%         results.nobs          = number of observations
%         results.nvar          = number of variables 
%         results.time1         = time for optimization
%         results.time          = total time taken
%         results.iter          = # of EGLS iterations taken
% ---------------------------------------------------
% %  SEE ALSO: prt_gmm(results), sem, sem_g
% ---------------------------------------------------
% REFERENCES: Luc Anselin Spatial Econometrics (1988) pages 182-183.
% Kelejian, H., and  Prucha, I.R.  (1998). A Generalized Spatial Two-Stage
% Least Squares Procedure for Estimating a Spatial Autoregressive
% Model with Autoregressive Disturbances. Journal of Real
% Estate and Finance Economics,  17, 99-121.
% Documentation in microsoft word format included in the Econometrics Toolbox
% GENERALIZED MOMENTS ESTIMATION FOR FLEXIBLE SPATIAL ERROR MODELS:  
% A LIBRARY FOR MATLAB, by Shawn Bucholtz
% ---------------------------------------------------

% written by: Shawn Bucholtz
% SBUCHOLTZ@ers.usda.gov
% USDA-ERS-ISD-ADB
% modified extensively by J.P. LeSage

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


% error checking on inputs
xsum = sum(x);
[n,k] = size(x);
ind = find(xsum == n);
iflag = 0;
if length(ind) > 0 % we have an intercept
    if ind ~= 1
    warning('sem_gmm: intercept must be in 1st column of the x-matrix');
    end;
    iflag = 1;
end;

if nargin == 4 % we need to parse user input options
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
  end;
 end;
end;
    


results.meth = 'sem_gmm';
time1 = 0; 
time2 = 0;

timet = clock; % start the clock for overall timing


%Estimated OLS to get a vector of residuals
[n nvar]=size(x);
results.nobs=n;
results.nvar=nvar;
o1=ols(y,x);
e=o1.resid;

%Make inital guesses at parameter vector;
lambdavec = [.5;o1.sige];

%Begin Interation
econverge = e;
criteria = 0.001;
converge = 1.0;
iter = 0;
itermax = 100;

t0 = clock;
eo = e;

if itflag ~= 0
while (converge > criteria & iter < itermax)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Input arguements into system of equations and moment
%conditions;
%The notation is simialar to the publication;
%se denotes e with a single dot (W*e);
%de denotes e with a double dot (W*W*e);

trWW = sum(sum(W.*W));

    se=W*e;
    de=W*se;
    Gn=zeros(3,3);
    Gn(1,1)=(2/n)*e'*se;Gn(1,2)=(-1/n)*se'*se; Gn(1,3)=1;
    Gn(2,1)=(2/n)*se'*de; Gn(2,2)=(-1/n)*de'*de; Gn(2,3)=(1/n)*trWW;
    Gn(3,1)=(1/n)*((e'*de)+(se'*se)); Gn(3,2)=(-1/n)*se'*de; Gn(3,3)=0;
    Gn2=[(1/n)*e'*e;(1/n)*se'*se;(1/n)*e'*se];

    %Pass arguments to MInZ function;
    [lambdahat,infoz2,stat]=minz(lambdavec,infoz2.func,infoz2,Gn,Gn2);

    lambdavec = [lambdahat(1);lambdahat(2)];
    
    %Estimate Parameters using EGLS;
    tmp = speye(n) - lambdahat(1)*sparse(W);
    xs = tmp*x;
    ys = tmp*y;
    results.beta = xs\ys;
    e = y - x*results.beta;

    converge = max(abs(e - econverge));%Check convergence
    econverge = e;

    iter = iter + 1;
end;

elseif itflag == 0
    trWW = sum(sum(W.*W));

    se=W*e;
    de=W*se;
    Gn=zeros(3,3);
    Gn(1,1)=(2/n)*e'*se;Gn(1,2)=(-1/n)*se'*se; Gn(1,3)=1;
    Gn(2,1)=(2/n)*se'*de; Gn(2,2)=(-1/n)*de'*de; Gn(2,3)=(1/n)*trWW;
    Gn(3,1)=(1/n)*((e'*de)+(se'*se)); Gn(3,2)=(-1/n)*se'*de; Gn(3,3)=0;
    Gn2=[(1/n)*e'*e;(1/n)*se'*se;(1/n)*e'*se];
    %Pass arguments to MInZ function;
    [lambdahat,infoz2,stat]=minz(lambdavec,infoz2.func,infoz2,Gn,Gn2);
    
    %Estimate Parameters using EGLS;
    tmp = speye(n) - lambdahat(1)*sparse(W);
    xs = tmp*x;
    ys = tmp*y;
    results.beta = xs\ys;

end;    
    
time1 = etime(clock,t0);
results.iter = iter;

%Compute stats from minimization;
e1 = Gn2-Gn*[lambdahat(1);lambdahat(1)^2;lambdahat(2)];
vare1 = std(e1)*std(e1);
stde = sqrt(vare1*diag(stat.Hi));
results.GMlambdatstat=lambdahat(1)./stde(1);
results.GMsige=lambdahat(2);

%Estimate remaining parameters using EGLS;

results.lambda=lambdahat(1);
results.yhat = x*results.beta;
results.resid = y - results.yhat;

B = speye(n) - results.lambda*sparse(W); 
et = B*e;
epe = et'*et;
results.sige = (1/n)*epe;
Bx = B*x;
xpx = (1/results.sige)*(Bx'*Bx);
results.tstat = results.beta./(sqrt(diag(inv(xpx))));
results.se = sqrt(diag(inv(xpx)));
results.bcov = inv(xpx);

% produce an std for rho following Kelejian-Prucha (2004)
a = (1/n)*trace(W'*W);
c = sqrt(1/(1+a*a));

J = [2*c*(de'*se - a*se'*eo)   -c*(de'*de - a*se'*se)
     de'*eo + se'*se           -de'*se               ];
J = (1/n)*J;
tmp = [1
       2*lambdahat(1)];
J = J * tmp;

% =============================
F = (1/n)*trace(W'*W);
C0 = sqrt(1/(1+F*F));

A1N = C0*(W'*W - F*speye(n));
A2N = W'*W;
sigh = results.sige*results.sige;

% trick for traces
trA1A1 = sum(sum((A1N+A1N')'.*(A1N+A1N')));
trA1A2 = sum(sum((A2N+A2N')'*(A1N+A1N')));
trA2A2 = sum(sum((A2N+A2N')'*(A2N+A2N')));

% psi11 = (sigh)*trace((A1N+A1N')*(A1N+A1N'))/(2*n);
% psi12 = (sigh)*trace((A2N+A2N')*(A1N+A1N'))/(2*n);
% psi21 = (sigh)*trace((A2N+A2N')*(A1N+A1N'))/(2*n);
% psi22 = (sigh)*trace((A2N+A2N')*(A2N+A2N'))/(2*n);

psi11 = (sigh)*trA1A1/(2*n);
psi12 = (sigh)*trA1A2/(2*n);
psi21 = (sigh)*trA1A2/(2*n);
psi22 = (sigh)*trA2A2/(2*n);

phihat = [psi11 psi12
          psi21 psi22];
      
% =============================
JJI = inv(J'*J);
omega = JJI*J'*phihat*J*JJI;
     
% =============================
results.lambdatstat = lambdahat(1)/sqrt(omega/n);
results.std_rho = sqrt(omega/n);
    
% R-squared calculation
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
