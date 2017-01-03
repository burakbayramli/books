function results=bcasetti(y,x,xc,yc,ndraw,nomit,option)
% PURPOSE: computes a heteroscedastic Bayesian variant of Casetti's spatial expansion regression
%          y = X*g + e, e = N(0,sige*V)
%          g = Zx*b0x + Zy*b0y, for x-y expansion
%          g = D b0,            for distance expansion
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r,          
%---------------------------------------------------
% USAGE: results = bcasetti(y,x,xc,yc,ndraw,nomit,option)
% where:       y = dependent variable vector
%              x = independent variables matrix
%             xc = latittude (or longitude) coordinate
%             yc = longitude (or latittude) coordinate
%          ndraw = # of draws
%          nomit = # of draws to omit for burn-in
%        option  = a structure variable containing options
%        option.exp  = 0 for x-y expansion (default)
%                    = 1 for distance from ctr expansion
%        option.ctr  = central point observation # for distance expansion
%        option.norm = 1 for isotropic x-y normalization (default=0)
%        option.rval = rval for prior (r above, default = 4)
%---------------------------------------------------
% RETURNS:
%        results.meth   = 'bcasetti'
%        results.b0draw = (underlying b0x, b0y draws) ndraw-nomit x nvar
%        results.beta   = mean of spatially expanded estimates (nobs x nvar)
%        results.vmean  = mean of vi draws (nobs x 1)
%        results.yhat   = mean of posterior predicted values
%        results.resid  = mean of residuals based on predicted values
%        results.sdraw  = sige draws (ndraw-nomit x 1)
%        results.rsqr   = rsquared
%        results.rbar   = rbar-squared
%        results.rval   = rval (from input)
%        results.nobs   = nobs
%        results.nvar   = # of variables in x
%        results.y      = y data vector
%        results.xc     = xc
%        results.yc     = yc
%        results.ctr    = ctr (if input)
%        results.dist   = distance vector (if ctr used)
%        results.exp    = exp input option
%        results.norm   = norm input option
%        results.time   = time taken for sampling        
% --------------------------------------------------
% NOTE: assumes x(:,1) contains a constant term
% --------------------------------------------------
% SEE ALSO: prt, plt, prt_spat()
%---------------------------------------------------
%
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

nflag = 0;

if nargin == 7 % user options
 if ~isstruct(option)
    error('bcasetti: must supply the option argument as a structure variable');
 else
 fields = fieldnames(option);
 nf = length(fields);
 % set defaults
 exp = 0; ctr = 0; nflag = 0; rval = 4;
  for i=1:nf
    if strcmp(fields{i},'exp')
        exp = option.exp; 
    elseif strcmp(fields{i},'ctr')
        ctr = option.ctr;
    elseif strcmp(fields{i},'norm')
      nflag = option.norm;
    elseif strcmp(fields{i},'rval')
      rval = option.rval;
    end;
  end; % end of for i
 end; % end of if else

elseif nargin == 6  % x-y expansion
exp = 0; rval = 4;
option.exp = 0;

else
error('Wrong # of arguments to bcasetti');
end;

if exp == 1
 if ctr == 0
   error('bcasetti: must enter option.ctr for option.exp = 1');
 end;
end;


[nobs nvar] = size(x);
if x(:,1) ~= ones(nobs,1)
 error('bcasetti: first column in x-matrix must be a constant vector');
end;

if nflag == 1
[xc yc] = normxy(xc,yc);
end;

results.meth = 'bcasetti';
results.y = y;
results.nobs = nobs;
results.nvar = nvar;
results.xc = xc;
results.yc = yc;
results.option = option;
results.exp = exp;
results.norm = nflag;
results.rval = rval;

switch exp

case {0} % x-y expansion

xt = x(:,2:nvar);
xx = matmul(xt,xc);
xy = matmul(xt,yc);
xmat = [x xx xy];
[junk nk] = size(xmat);
xpxi = inv(xmat'*xmat);
V = ones(nobs,1);
in = ones(nobs,1);
sige = 1;
yhmean = zeros(nobs,1);
vmean = zeros(nobs,1);
bhmean = zeros(nobs,2*(nvar-1));
t0 = clock;
for iter=1:ndraw; % begin sampling
xmatv = matmul(xmat,sqrt(V));
yv = y.*sqrt(V);
% update b0
xpxi = inv(xmatv'*xmatv);
xpy = xmatv'*yv;
b0 = xpxi*xpy;
a = chol(xpxi);
b0 = sqrt(sige)*a'*randn(nk,1) + b0;
% update sige 
e = yv - xmatv*b0;
chi = chis_rnd(1,nobs);
t2 = chi/(e'*e);
sige = 1/t2;
% update vi
e = y - xmat*b0;
chiv = chis_rnd(nobs,rval+1);   
vi = ((e.*e./sige) + in*rval)./chiv;
V = in./vi;   
% generate expansion estimates  
beta = zeros(nobs,2*(nvar-1));
yhat = zeros(nobs,1);
xx = matmul(ones(nobs,nvar-1),xc);
xy = matmul(ones(nobs,nvar-1),yc);
xxxy = [x xx xy];
tvar = length(b0);
    yhat(:,1) =  xmat(:,1:nvar)*b0(1:nvar,1);
    for j=nvar+1:tvar
    beta(:,j-nvar) = xxxy(:,j)*b0(j,1);
    yhat(:,1) = yhat(:,1)  + xmat(:,j)*b0(j,1);
    end;

if iter > nomit, % save draws
    vmean = vmean + vi;
    bsave(iter-nomit,:) = b0';
    ssave(iter-nomit,1) = sige;
    yhmean = yhmean + yhat;
    bhmean = bhmean + beta;
end;

end; % end of sampling loop
gtime = etime(clock,t0);

vmean = vmean/(ndraw-nomit);
yhat = yhmean/(ndraw-nomit);
beta = bhmean/(ndraw-nomit);

results.beta = beta;     % mean of expansion estimates
results.yhat = yhat;     % mean of yhat
e = results.y - results.yhat;
results.resid = e;       % residuals

results.b0draw = bsave;
results.sdraw = ssave;
results.vmean = vmean;
results.time = gtime;
results.ndraw = ndraw;
results.nomit = nomit;

case{1} % distance from the center expansion


% compute distance from central point
xi = xc(ctr);
yi = yc(ctr);
% calculate distance weighting function
d = sqrt((xc-xi).*(xc-xi) + (yc-yi).*(yc-yi));
dvec = d;
results.dist= dvec;

% transform x-variables using distance vector
xt = x(:,2:nvar);
xx = matmul(xt,dvec);
xmat = [x xx];
[junk tvar] = size(xmat);
V = ones(nobs,1);
in = ones(nobs,1);
sige = 1;
yhmean = zeros(nobs,1);
vmean = zeros(nobs,1);
bhmean = zeros(nobs,nvar-1);

t0 = clock;
for iter=1:ndraw; % begin sampling
xmatv = matmul(xmat,sqrt(V));
yv = y.*sqrt(V);

% update b0
xpxi = inv(xmatv'*xmatv);
xpy = xmatv'*yv;
b0 = xpxi*xpy;
a = chol(xpxi);
b0 = sqrt(sige)*a'*randn(length(b0),1) + b0;
% update sige 
e = yv - xmatv*b0;
chi = chis_rnd(1,nobs);
t2 = chi/(e'*e);
sige = 1/t2;
% update vi
e = y - xmat*b0;
chiv = chis_rnd(nobs,rval+1);   
vi = ((e.*e./sige) + in*rval)./chiv;
V = in./vi;   

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

if iter > nomit, % save draws
    vmean = vmean + vi;
    bsave(iter-nomit,:) = b0';
    ssave(iter-nomit,1) = sige;
    yhmean = yhmean + yhat;
    bhmean = bhmean + beta;
end;

end; % end of sampling loop
gtime = etime(clock,t0);

vmean = vmean/(ndraw-nomit);
yhat = yhmean/(ndraw-nomit);
beta = bhmean/(ndraw-nomit);

results.vmean = vmean;
results.beta = beta;
results.yhat = yhat;
results.resid = y - yhat;
results.b0draw = bsave;
results.nvar = nvar;
results.ctr = ctr;
results.sdraw = ssave;
results.yhat = yhat;
results.rval = rval;
results.time = gtime;
results.ndraw = ndraw;
results.nomit = nomit;

otherwise
error('casetti: check option input argument');
end;
