function result = gwr(y,x,east,north,info);
% PURPOSE: compute geographically weighted regression
%----------------------------------------------------
% USAGE: results = gwr(y,x,east,north,info)
% where:   y = dependent variable vector
%          x = explanatory variable matrix
%       east = x-coordinates in space
%      north = y-coordinates in space
%       info = a structure variable with fields:
%       info.bwidth = scalar bandwidth to use or zero 
%                     for cross-validation estimation (default)
%       info.bmin   = minimum bandwidth to use in CV search
%       info.bmax   = maximum bandwidth to use in CV search
%                     defaults: bmin = 0.1, bmax = 20                                             
%       info.dtype  = 'gaussian'    for Gaussian weighting (default)
%                   = 'exponential' for exponential weighting
%                   = 'tricube'     for tri-cube weighting
%       info.q      = q-nearest neighbors to use for tri-cube weights
%                     (default: CV estimated)  
%       info.qmin   = minimum # of neighbors to use in CV search
%       info.qmax   = maximum # of neighbors to use in CV search
%                     defaults: qmin = nvar+2, qmax = 4*nvar     
% ---------------------------------------------------                                    
%  NOTE: res = gwr(y,x,east,north) does CV estimation of bandwidth
% ---------------------------------------------------
% RETURNS: a results structure
%        results.meth  = 'gwr'
%        results.beta  = bhat matrix    (nobs x nvar)
%        results.tstat = t-stats matrix (nobs x nvar)
%        results.yhat  = yhat
%        results.resid = residuals
%        results.sige  = e'e/(n-dof) (nobs x 1)
%        results.nobs  = nobs
%        results.nvar  = nvars
%        results.bwidth  = bandwidth if gaussian or exponential
%        results.q       = q nearest neighbors if tri-cube
%        results.dtype   = input string for Gaussian, exponential weights
%        results.iter    = # of simplex iterations for cv
%        results.north = north (y-coordinates)
%        results.east  = east  (x-coordinates)
%        results.y     = y data vector
%---------------------------------------------------
% See also: prt,plt, prt_gwr, plt_gwr to print and plot results
%---------------------------------------------------
% References: Brunsdon, Fotheringham, Charlton (1996)
% Geographical Analysis, pp. 281-298
%---------------------------------------------------
% NOTES: uses auxiliary function scoref for cross-validation
%---------------------------------------------------

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin == 5 % user options
 if ~isstruct(info)
    error('gwr: must supply the option argument as a structure variable');
 else
 fields = fieldnames(info);
 nf = length(fields);
 % set defaults
 [n k] = size(x);
 bwidth = 0; dtype = 0; q = 0; qmin = k+2; qmax = 5*k; 
 bmin = 0.1; bmax = 20.0;
  for i=1:nf
    if strcmp(fields{i},'bwidth')
        bwidth = info.bwidth; 
    elseif strcmp(fields{i},'dtype')
        dstring = info.dtype;
       if strcmp(dstring,'gaussian')
        dtype = 0;
       elseif strcmp(dstring,'exponential')
        dtype = 1;
       elseif strcmp(dstring,'tricube')
        dtype = 2;
       end;
    elseif strcmp(fields{i},'q')
       q = info.q;
    elseif strcmp(fields{i},'qmax');
        qmax = info.qmax;
    elseif strcmp(fields{i},'qmin');
        qmin = info.qmin;
    elseif strcmp(fields{i},'bmin');
        bmin = info.bmin;
    elseif strcmp(fields{i},'bmax');
        bmax = info.bmax;

    end; 
  end; % end of for i
 end; % end of if else

elseif nargin == 4
 bwidth = 0; dtype = 0; dstring = 'gaussian';
        bmin = 0.1; bmax = 20.0;
else
 error('Wrong # of arguments to gwr');
end;


% error checking on inputs
[nobs nvar] = size(x);
[nobs2 junk] = size(y);
[nobs3 junk] = size(north);
[nobs4 junk] = size(east);

result.north = north;
result.east = east;

if nobs ~= nobs2
 error('gwr: y and x must contain same # obs');
elseif nobs3 ~= nobs
 error('gwr: north coordinates must equal # obs');
elseif nobs3 ~= nobs4
 error('gwr: east coordinates must equal # in north');
end;

switch dtype

case{0,1} % bandwidth cross-validation
if bwidth == 0 % cross-validation

options = optimset('fminbnd');
optimset('MaxIter',500);

if dtype == 0     % Gaussian weights
[bdwt,junk,exitflag,output] = fminbnd('scoref',bmin,bmax,options,y,x,east,north,dtype);
elseif dtype == 1 % exponential weights
[bdwt,junk,exitflag,output] = fminbnd('scoref',bmin,bmax,options,y,x,east,north,dtype);
end;        
 if output.iterations == 500, 
 fprintf(1,'gwr: cv convergence not obtained in %4d iterations',output.iterations);
 else
 result.iter = output.iterations;
 end;

else
 bdwt = bwidth*bwidth; % user supplied bandwidth
end;

case{2} % q-nearest neigbhor cross-validation
if q == 0 % cross-validation
q = scoreq(qmin,qmax,y,x,east,north);

else
% use user-supplied q-value
end;

otherwise

end;

% do GWR using bdwt as bandwidth
[n k] = size(x);
bsave = zeros(n,k);
ssave = zeros(n,k);
sigv  = zeros(n,1);
yhat  = zeros(n,1);
resid = zeros(n,1);
wt = zeros(n,1);
d = zeros(n,1);
for iter=1:n;
    dx = east - east(iter,1);
    dy = north - north(iter,1);
    d = (dx.*dx + dy.*dy);
    sd = std(sqrt(d));
    % sort distance to find q nearest neighbors
    ds = sort(d); 
    if dtype == 2, dmax = ds(q,1); end;
       if dtype == 0,     % Gausian weights 
        wt = stdn_pdf(sqrt(d)/(sd*bdwt));
       elseif dtype == 1, % exponential weights
        wt = exp(-d/bdwt);
       elseif dtype == 2, % tricube weights
 wt = zeros(n,1);
 nzip = find(d <= dmax);
        wt(nzip,1) = (1-(d(nzip,1)/dmax).^3).^3;
       end; % end of if,else 
wt = sqrt(wt);


% computational trick to speed things up
% use non-zero wt to pull out y,x observations
nzip = find(wt >= 0.01);
ys = y(nzip,1).*wt(nzip,1);
xs = matmul(x(nzip,:),wt(nzip,1));
xpxi = invpd(xs'*xs);
b = xpxi*xs'*ys;
% compute predicted values
yhatv = xs*b;
yhat(iter,1) = x(iter,:)*b;
resid(iter,1) = y(iter,1) - yhat(iter,1);
% compute residuals 
e = ys - yhatv;
% find # of non-zero observations
nadj = length(nzip);
sige = (e'*e)/nadj;

% compute t-statistics
sdb = sqrt(sige*diag(xpxi));
% store coefficient estimates and std errors in matrices
% one set of beta,std for each observation
bsave(iter,:) = b';
ssave(iter,:) = sdb'; 
sigv(iter,1) = sige;
end;


% fill-in results structure
result.meth = 'gwr';
result.nobs = nobs;
result.nvar = nvar;
if (dtype == 0 | dtype == 1)
result.bwidth = sqrt(bdwt);
else
result.q = q;
end;
result.beta = bsave;
result.tstat = bsave./ssave;
result.sige = sigv;
result.dtype = dstring;
result.y = y;
result.yhat = yhat;
% compute residuals and conventional r-squared
result.resid = resid;
sigu = result.resid'*result.resid;
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
result.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);
result.rbar = 1 - (rsqr1/rsqr2); % rbar-squared


