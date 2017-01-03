function results=darp(y,x,xc,yc,option)
% PURPOSE: computes Casetti's DARP model
%---------------------------------------------------
% USAGE: results = darp(y,x,xc,yc,option)
% where:       y = dependent variable vector
%              x = independent variables matrix
%             xc = lattitude (or longitude) coordinate
%             yc = longitude (or lattitude) coordinate
%        option  = a structure variable containing options
%        option.exp  = 0 for x-y expansion (default)
%                    = 1 for distance from ctr expansion
%        option.ctr  = central point observation # for distance expansion
%        option.iter = # of iterations for maximum likelihood routine
%        option.norm = 1 for isotropic x-y normalization (default=0)        
%---------------------------------------------------
% RETURNS:
%        results.meth   = 'darp'
%        results.b0     = bhat (underlying b0x, b0y)
%        results.t0     = t-stats (associated with b0x, b0y)
%        results.beta   = spatially expanded estimates (nobs x nvar)
%        results.yhat   = yhat
%        results.resid  = residuals
%        results.sige   = e'*e/(n-k)
%        results.rsqr   = rsquared
%        results.rbar   = rbar-squared
%        results.nobs   = nobs
%        results.nvar   = # of variables in x
%        results.y      = y data vector
%        results.xc     = xc
%        results.yc     = yc
%        results.ctr    = ctr (if input)
%        results.dist   = distance vector 
%        results.exp    = exp input option
%        results.norm   = norm input option
%        results.iter   = # of maximum likelihood iterations
% --------------------------------------------------
% NOTES: assumes x(:,1) contains a constant term
% --------------------------------------------------
% SEE ALSO: prt, plt, prt_spat()
%---------------------------------------------------
% REFERENCES: Emilio Casetti (1982) ``Drift Analysis of
% Regression Parameters: An Application to the Investigation
% of Fertility Development Relations'', Modeling and Simulation 13,
% Part 3: pp. 961-66.
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin == 5 % user options
 if ~isstruct(option)
    error('darp: must supply the option argument as a structure variable');
 else
 fields = fieldnames(option);
 nf = length(fields);
 % set defaults
 expand = 0; ctr = 0; iter = 0; nflag = 0;
  for i=1:nf
    if strcmp(fields{i},'exp')
        expand = option.exp; 
    elseif strcmp(fields{i},'ctr')
        ctr = option.ctr;
    elseif strcmp(fields{i},'iter')
        iter = option.iter;
    elseif strcmp(fields{i},'norm')
        nflag = option.norm;
    end;
  end; % end of for i
 end; % end of if else

elseif nargin == 4  % x-y expansion
expand = 0;
option.exp = 0;
iter = 0;
nflag = 0;

else
error('Wrong # of arguments to darp');
end;

if expand == 1
 if ctr == 0
   error('darp: must enter option.ctr for option.exp = 1');
 end;
end;


[nobs nvar] = size(x);
if x(:,1) ~= ones(nobs,1)
 error('darp: first column in x-matrix must be a constant vector');
end;

if nflag == 1
[xc yc] = normxy(xc,yc);
end;

results.meth = 'darp';
results.y = y;
results.nobs = nobs;
results.nvar = nvar;
results.xc = xc;
results.yc = yc;
results.option = option;
results.exp = expand;
results.norm = nflag;

switch expand

case {0} % x-y expansion
    
% get base estimates
xt = x(:,2:nvar);
xx = matmul(xt,xc);
xy = matmul(xt,yc);
xmat = [x xx xy];
b0 = xmat\y;
% compute expansion estimates and predicted values
beta = zeros(nobs,2*(nvar-1));
yhat = zeros(nobs,1);
xx = matmul(ones(nobs,nvar-1),xc);
xy = matmul(ones(nobs,nvar-1),yc);
xxxy = [x xx xy];
tvar = length(b0);
    yhat(:,1) = xmat(:,1:nvar)*b0(1:nvar,1);
    for j=nvar+1:tvar
    beta(:,j-nvar) = xxxy(:,j)*b0(j,1);
    yhat(:,1) = yhat(:,1)  + xmat(:,j)*b0(j,1);
    end;

% use residuals to provide an initial set of FGLS estimates
e = results.y - yhat;
e2 = log(e.*e);
% regress residuals on xc,yc vectors
n = length(e);
res = ols(e2,[ones(n,1) xc yc]);
% pull out sige, gamma1
sige = res.beta(1,1);
gamma1 = res.beta(2,1);
gamma2 = res.beta(3,1);
% do FGLS
xt = x(:,2:nvar);
xx = matmul(xt,xc);
xy = matmul(xt,yc);
xmat = [x xx xy];
phi = exp(sige + gamma1*xc + gamma2*yc);
phii = ones(n,1)./phi;
ys = sqrt(phii).*y;
xs = matmul(xmat,sqrt(phii));
b0 = xs\ys; % FGLS estimates

% do maximum likelihood estimation
if iter == 0
info.maxit = 500;
else
info.maxit = iter;
end;
parm = zeros(tvar+3,1);
parm(1,1) = sige;
parm(2,1) = gamma1;
parm(3,1) = gamma2;
parm(4:tvar+3,1) = b0;
result = maxlik('darp_lik1',parm,info,y,x,xc,yc);

if result.iter == info.maxit
    % warn user and rely on FGLS estimates
fprintf(1,'darp: convergence failure using FGLS estimates');
results.b0 = result.b(4:tvar+3,1);
results.gamma(1) = result.b(2,1);
results.gamma(2) = result.b(3,1);
results.iter = result.iter; result.lik = 0;
fflag = 1; % a flag for FGLS estimates
else
results.b0 = result.b(4:tvar+3,1);
sige = result.b(1,1);
results.gamma(1) = result.b(2,1);
gamma1 = result.b(2,1);
results.gamma(2) = result.b(3,1);
gamma2 = result.b(3,1);
results.iter = iter;  
results.lik = result.f;
fflag = 0; % a flag for ML estimates
end;

% compute FGLS or ML expansion  estimates and predicted values
beta = zeros(nobs,2*(nvar-1));
yhat = zeros(nobs,1);
xx = matmul(ones(nobs,nvar-1),xc);
xy = matmul(ones(nobs,nvar-1),yc);
xxxy = [x xx xy];
tvar = length(b0);
    yhat(:,1) = xmat(:,1:nvar)*b0(1:nvar,1);
    for j=nvar+1:tvar
    beta(:,j-nvar) = xxxy(:,j)*b0(j,1);
    yhat(:,1) = yhat(:,1)  + xmat(:,j)*b0(j,1);
    end;

results.beta = beta;     % expansion estimates
results.yhat = yhat;     % yhat
e = y - results.yhat;
results.resid = e;       % residuals
sigu = e'*e;
results.sige = sigu/(n-tvar);
% compute t-statistics for bhat's
phi = exp(sige + gamma1*xc + gamma2*yc);
phii = ones(n,1)./phi;
xs = matmul(xmat,sqrt(phii));
xpxi = inv(xs'*xs);
tmp2 = sige*(diag(xpxi));
results.t0 = results.b0./(sqrt(tmp2));
% compute chi-squared(1) statistic for gamma1,gamma2
if fflag == 0,
    results.chi(1) = (gamma1*gamma1)/2*sum(xc);
    results.chi(2) = (gamma2*gamma2)/2*sum(yc);
results.cprob(1) = 1-chis_prb(results.chi(1),1);
results.cprob(2) = 1-chis_prb(results.chi(2),1);    
else
    results.chi(1) = (gamma1*gamma1)/4.9348*sum(xc);
    results.chi(2) = (gamma2*gamma2)/4.9348*sum(yc);
results.cprob(1) = 1-chis_prb(results.chi(1),1);
results.cprob(2) = 1-chis_prb(results.chi(2),1);
end;

ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar-2*(nvar-1));
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared


case{1} % distance from the center expansion

% compute squared distance from central point
xi = xc(ctr);
yi = yc(ctr);
% calculate distance weighting function
d = (xc-xi).*(xc-xi) + (yc-yi).*(yc-yi);
dvec = d;
results.dist= dvec;

% transform x-variables using distance vector
xt = x(:,2:nvar);
xx = matmul(xt,dvec);
xmat = [x xx];
b0 = xmat\y;  % get base model estimates

% find FGLS expansion estimates and  residuals
beta = zeros(nobs,(nvar-1));
yhat = zeros(nobs,1);
xx = matmul(ones(nobs,nvar-1),dvec);
xxxy = [x xx];
tvar = length(b0);
    yhat(:,1) = xmat(:,1:nvar)*b0(1:nvar,1);
    for j=nvar+1:tvar
    beta(:,j-nvar) = xxxy(:,j)*b0(j,1);
    yhat(:,1) = yhat(:,1)  + xmat(:,j)*b0(j,1);
    end;

% use residuals to provide an initial set of FGLS estimates
e = y - yhat;
e2 = log(e.*e);
% regress residuals on distance vector
n = length(e);
res = ols(e2,[ones(n,1) dvec]);
% pull out sige, gamma1
sige = res.beta(1,1);
gamma1 = res.beta(2,1);
% do FGLS
xt = x(:,2:nvar);
xx = matmul(xt,dvec);
xmat = [x xx];
phi = exp(sige + gamma1*dvec);
phii = ones(n,1)./phi;
ys = sqrt(phii).*y;
xs = matmul(xmat,sqrt(phii));
b0 = xs\ys; % FGLS estimates

% do maximum likelihood estimation
if iter == 0
info.maxit = 500;
else
info.maxit = iter;
end;
parm = zeros(tvar+2,1);
parm(1,1) = sige;
parm(2,1) = gamma1;
parm(3:tvar+2,1) = b0;
result = maxlik('darp_lik2',parm,info,y,x,dvec);
if result.iter == info.maxit;
    % warn user and rely on FGLS estimates
fprintf(1,'darp: convergence failure --- returning FGLS estimates\n');
results.b0 = result.b(3:tvar+2,1);
results.gamma(1) = result.b(2,1);
results.iter = result.iter;  
results.lik = 0;
fflag = 1;
else
results.b0 = result.b(3:tvar+2,1);
sige = result.b(1,1);
results.gamma(1) = result.b(2,1);
results.iter = result.iter;  
results.lik = result.f;
gamma1 = result.b(2,1);
fflag = 0;
end;

% find FGLS or ML expansion estimates and  residuals
beta = zeros(nobs,(nvar-1));
yhat = zeros(nobs,1);
xx = matmul(ones(nobs,nvar-1),dvec);
xxxy = [x xx];
tvar = length(b0);
    yhat(:,1) = xmat(:,1:nvar)*b0(1:nvar,1);
    for j=nvar+1:tvar
    beta(:,j-nvar) = xxxy(:,j)*b0(j,1);
    yhat(:,1) = yhat(:,1)  + xmat(:,j)*b0(j,1);
    end;

results.beta = beta;
results.yhat = yhat;
results.nvar = nvar;
results.ctr = ctr;

results.resid = y - results.yhat;
sigu = results.resid'*results.resid;
results.sige = sigu/(n-tvar);
phi = exp(sige + gamma1*dvec);
phii = ones(n,1)./phi;
xs = matmul(xmat,sqrt(phii));
xpxi = inv(xs'*xs);
tmp = sige*(diag(xpxi));
results.t0 = results.b0./(sqrt(tmp));
% compute chi-squared(1) statistic for gamma1
if fflag == 0,
    results.chi(1) = (gamma1*gamma1)/2*sum(dvec);
results.cprob(1) = 1-chis_prb(results.chi(1),1);
else
    results.chi(1) = (gamma1*gamma1)/4.9348*sum(dvec);
results.cprob(1) = 1-chis_prb(results.chi(1),1);
end;

ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-2*nvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared

otherwise
error('darp: check option input argument');
end;
