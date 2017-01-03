function result = gwr_probit(y,x,east,north,info);
% PURPOSE: compute geographically weighted regression
%          for the probit model
%----------------------------------------------------
% USAGE: results = gwr(y,x,east,north,info)
% where:   y = variable vector with 0,1 values
%          x = explanatory variable matrix
%       east = x-coordinates in space
%      north = y-coordinates in space
%       info = a structure variable with fields:
%       info.bwidth = scalar bandwidth to use 
%                     (default = cross-validation estimate)   
%       info.bmin   = minimum bandwidth to use in CV search
%       info.bmax   = maximum bandwidth to use in CV search  
%                       defaults: bmin = 0.1, bmax = 20                                                                 
% ---------------------------------------------------                                    
% NOTES: res = gwr_probit(y,x,east,north) does CV estimation of bandwidth
%        Uses Gaussian weighting, and scoref_prob for CV
% ---------------------------------------------------
% RETURNS: a results structure
%        results.meth  = 'gwr_probit' 
%        results.beta  = bhat matrix    (nobs x nvar)
%        results.tstat = t-stats matrix (nobs x nvar)
%        results.yhat  = yhat
%        results.resid = residuals
%        results.sige  = e'e/(n-dof) (nobs x 1)
%        results.rsqr  = Estrella R^2
%        results.lik   = -Log likelihood
%        results.nobs  = nobs
%        results.nvar  = nvars
%        results.bwidth= bandwidth if gaussian or exponential
%        results.iter   = # of simplex iterations for cv
%        results.north = north (y-coordinates)
%        results.east  = east  (x-coordinates)
%        results.y     = y data vector
%        results.one   = # of 1's
%        results.zip  = # of 0's
%---------------------------------------------------
% See also: prt,plt, prt_gwr, plt_gwr to print and plot results
%---------------------------------------------------
% References: Brunsdon, Fotheringham, Charlton (1996)
% Geographical Analysis, pp. 281-298
%---------------------------------------------------
% NOTES: uses auxiliary function scoref_prob for cross-validation
%---------------------------------------------------

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin == 5 % user options
 if ~isstruct(info)
    error('gwr_probit: must supply the option argument as a structure variable');
 else
 fields = fieldnames(info);
 nf = length(fields);
 % set defaults
 [n k] = size(x); 
 bwidth = 0; dtype = 1; 
bmin = 0.1; bmax = 20;  % default values for CV gaussian and exponential search 
  for i=1:nf
    if strcmp(fields{i},'bwidth')
        bwidth = info.bwidth; 
    elseif strcmp(fields{i},'bmin');
        bmin = prior.bmin;   
    elseif strcmp(fields{i},'bmax');
        bmax = prior.bmax; 
    end; 
  end; % end of for i
 end; % end of if else

elseif nargin == 4
        bwidth = 0; dtype = 1;
        bmin = 0.1; bmax = 20.0;
else
 error('Wrong # of arguments to gwr_probit');
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


if bwidth == 0 % cross-validation

options = optimset('fminbnd');
optimset('MaxIter',500);


[bdwt,junk,exitflag,output] = fminbnd('scoref_prob',bmin,bmax,options,y,x,east,north);       
 if output.iterations == 500, 
 fprintf(1,'gwr_probit: cv convergence not obtained in %4d iterations',output.iterations);
 else
 result.iter = output.iterations;
 end;

else
 bdwt = bwidth*bwidth; % user supplied bandwidth
end;


% do GWR using bdwt as bandwidth
[n k] = size(x);
bsave = zeros(n,k);
ssave = zeros(n,k);
sigv  = zeros(n,1);
yhat  = zeros(n,1);
resid = zeros(n,1);
tstat = zeros(n,k);
like = zeros(n,1);

wt = zeros(n,1);
d = zeros(n,1);
for iter=1:n;
    dx = east - east(iter,1);
    dy = north - north(iter,1);
    d = (dx.*dx + dy.*dy);
    % Gausian weights 
    sd = std(sqrt(d));
    wt = stdn_pdf(sqrt(d)/(sd*bdwt)); 
wt = sqrt(wt);     
xs = matmul(x,wt);
res = probit(y,xs);
bsave(iter,:) = res.beta';
yhat(iter,1) = norm_cdf(x(iter,:)*res.beta);
resid(iter,1) = y(iter,1) - yhat(iter,1);
sigv(iter,1) = res.sige;
tstat(iter,:) = res.tstat';
like(iter,1) = res.lik;
end;

result.meth = 'gwr_probit';
result.nobs = nobs;
result.nvar = nvar;
result.bwidth = sqrt(bdwt);
result.beta = bsave;
result.tstat = tstat;
result.sige = sigv;
result.y = y;
result.yhat = yhat;
tmp = find(y == 1); % compute Estrella R-squared
P = length(tmp);
result.one = P;
result.zip = nobs-P;

cnt0 = nobs-P;
cnt1 = P;
P = P/nobs;
like0 = nobs*(P*log(P) + (1-P)*log(1-P));
like1 = sum(abs(like));
term0 = (2/nobs)*like0;
term1 = 1/(abs(like1)/abs(like0))^term0;
result.rsqr = 1-term1;
result.resid = resid;
result.lik = like1;
