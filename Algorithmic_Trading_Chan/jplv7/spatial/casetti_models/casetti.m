function results=casetti(y,x,xc,yc,option)
% PURPOSE: computes Casetti's spatial expansion regression
%---------------------------------------------------
% USAGE: results = casetti(y,x,xc,yc,option)
% where:       y = dependent variable vector
%              x = independent variables matrix
%             xc = latittude (or longitude) coordinate
%             yc = longitude (or latittude) coordinate
%        option  = a structure variable containing options
%        option.exp  = 0 for x-y expansion (default)
%                    = 1 for distance from ctr expansion
%        option.ctr  = central point observation # for distance expansion
%        option.norm = 1 for isotropic x-y normalization (default=0)
%---------------------------------------------------
% RETURNS:
%        results.meth   = 'casetti'
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
%        results.dist   = distance vector (if ctr used)
%        results.exp    = exp input option
%        results.norm   = norm input option
% --------------------------------------------------
% NOTE: assumes x(:,1) contains a constant term
% --------------------------------------------------
% SEE ALSO: prt, plt, prt_spat()
%---------------------------------------------------
% REFERENCES: Casetti, E., (1972) 'Generating Models by the Expansion Method: 
% Applications to Geographic Research', Geographical Analysis, Vol. 4, pp. 81-91.
% Casetti, E., (1992) 'Bayesian Regression and the Expansion Method',
% Geographical Analysis, Vol. 24, pp. 58-74.
% Casetti, E., and J.P. Jones, III (1987) 'Spatial Applications of the
% Expansion Method Paradigm' in Quantitative Analysis in Geography,
% edited by C. Dufournaud and D. Dudycha, pp. 121-36. University of Waterloo,
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

nflag = 0;

if nargin == 5 % user options
 if ~isstruct(option)
    error('casetti: must supply the option argument as a structure variable');
 else
 fields = fieldnames(option);
 nf = length(fields);
 % set defaults
 exp = 0; ctr = 0; nflag = 0;
  for i=1:nf
    if strcmp(fields{i},'exp')
        exp = option.exp; 
    elseif strcmp(fields{i},'ctr')
        ctr = option.ctr;
    elseif strcmp(fields{i},'norm')
      nflag = option.norm;
    end;
  end; % end of for i
 end; % end of if else

elseif nargin == 4  % x-y expansion
exp = 0;
option.exp = 0;

else
error('Wrong # of arguments to casetti');
end;

if exp == 1
 if ctr == 0
   error('casetti: must enter option.ctr for option.exp = 1');
 end;
end;


[nobs nvar] = size(x);
if x(:,1) ~= ones(nobs,1)
 error('casetti: first column in x-matrix must be a constant vector');
end;

if nflag == 1
[xc yc] = normxy(xc,yc);
end;

results.meth = 'casetti';
results.y = y;
results.nobs = nobs;
results.nvar = nvar;
results.xc = xc;
results.yc = yc;
results.option = option;
results.exp = exp;
results.norm = nflag;

switch exp

case {0} % x-y expansion

xt = x(:,2:nvar);
xx = matmul(xt,xc);
xy = matmul(xt,yc);
xmat = [x xx xy];
b0 = xmat\y;
xpxi = inv(xmat'*xmat);
results.b0 = b0;                      % bhat's

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
e = results.y - results.yhat;
results.resid = e;       % residuals
sigu = e'*e;
results.sige = sigu/(nobs-tvar);

tmp2 = results.sige*(diag(xpxi));
results.t0 = results.b0./(sqrt(tmp2));

ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar-2*(nvar-1));
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared


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
b0 = xmat\y;
xpxi = inv(xmat'*xmat);
results.b0 = b0;                      % bhat's

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
results.b0 = b0;
results.nvar = nvar;
results.ctr = ctr;

results.resid = y - results.yhat;
sigu = results.resid'*results.resid;
results.sige = sigu/(nobs-tvar);
xpxi = inv(xmat'*xmat);
    tmp = results.sige*(diag(xpxi));
    results.t0 = results.b0./(sqrt(tmp));
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-2*nvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared

otherwise
error('casetti: check option input argument');
end;
