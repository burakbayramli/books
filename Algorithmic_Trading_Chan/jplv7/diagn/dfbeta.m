function results=dfbeta(y,x)
% PURPOSE: computes BKW (influential observation diagnostics)
%          dfbetas, dffits, hat-matrix, studentized residuals
%---------------------------------------------------
% USAGE: result = dfbeta(y,x)
% where: y = dependent variable vector (from a regression model)
%        x = independent variable matrix
%---------------------------------------------------
% RETURNS: a structure
%        results.meth   = 'dfbeta'
%        results.dfbeta = df betas
%        results.dffits = df fits
%        results.hatdi  = hat-matrix diagonals
%        results.stud   = studentized residuals
%        results.nobs   = # of observations
%        results.nvar   = # of variables in x-matrix
% --------------------------------------------------
% SEE ALSO: plt, plt_dfb, plt_dff,  bkw, diagnose, rdiag
%---------------------------------------------------
% REFERENCE: Belsley, Kuh, Welsch, 1980 Regression Diagnostics

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatia-econometrics.com


[nobs nvar] = size(x);
results.nobs = nobs;
results.nvar = nvar;
results.meth = 'dfbeta';

if (nargin ~= 2)
error('Wrong number of arguments to dfbeta');
end;


% perform QR decomposition
[q r] = qr(x);
% pull out first nvar columns of q'
qt = q(1:nvar,:);

% get OLS estimates and related stuff
bols = r\(q'*y);
e = y - x*bols;
sige = e'*e/(nobs-nvar);
e2 = e.*e;

% find hat-matrix
h = zeros(nobs,1);
for i=1:nobs;
h(i,1) = qt(:,i)'*qt(:,i);
end;

omh = ones(nobs,1) - h;
homh = sqrt(h./omh);

d1 = (nobs-nvar)/(nobs-nvar-1)*sige*ones(nobs,1);
d2 = e2./((nobs-nvar-1)*omh);
si = sqrt(d1-d2);
t1 = si.*sqrt(omh);

dffits = homh.*(e./t1);

g = e./(omh);
g = diag(g);

dfb = r'\(qt*g);
dfbeta = zeros(nobs,nvar);
dfbeta = dfb(1:nvar,1:nobs)';

sxx = sqrt(inv(x'*x));

for i=1:nvar;
scale(:,i) = si*sxx(i,i);
end;

dfbetas = dfbeta./scale;

results.dfbeta = dfbeta;
results.dffits = dffits;
results.hatdi = h;
results.stud = e./(si.*sqrt(omh));

