function results = bgwrv(y,x,east,north,ndraw,nomit,info);
% PURPOSE: compute Bayesian robust geographically weighted regression
%          Wi*y = Wi*X*bi + ei, ei is N(0,sigi*Vi)
%          Vi = diag(v1i,v2i,...vni), 
%          r/vi = ID chi(r)/r, r = Gamma(m,k) 
%----------------------------------------------------
% USAGE: results = bgwrv(y,x,east,north,ndraw,nomit,info)
% where:   y = dependent variable vector
%          x = explanatory variable matrix
%       east = x-coordinates in space
%      north = y-coordinates in space
%      ndraw = # of draws
%      nomit = # of initial draws omitted for burn-in
%       info = a structure variable with fields:
%       info.rval, r prior hyperparameter, default=4
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
%                     defaults: qmin = nvar+2, qmax = 5*nvar     
% ---------------------------------------------------                                    
%  NOTE: res = bgwrv(y,x,east,north) does CV estimation of bandwidth
% ---------------------------------------------------
% RETURNS: a results structure
%        results.meth   = 'bgwrv'
%        results.bdraw  = beta draws (ndraw-nomit x nobs x nvar) (a 3-d matrix)
%        results.smean  = mean of sige draws (nobs x 1)
%        results.vmean  = mean of vi draws (nobs x 1)
%        results.nobs   = nobs
%        results.nvar   = nvars
%        results.ndraw  = ndraw
%        results.nomit  = nomit        
%        results.r      = rval (from input)
%        results.bwidth = bandwidth if gaussian or exponential
%        results.q      = q nearest neighbors if tri-cube
%        results.dtype  = input string for Gaussian,exponential,tricube weights
%        results.iter   = # of simplex iterations for cv
%        results.ycoord = north (y-coordinates)
%        results.xcoord = east  (x-coordinates)
%        results.y      = y data vector
%        results.x      = x data matrix
%        results.time   = time (in seconds) taken
%---------------------------------------------------
% See also: prt,plt, prt_gwr, plt_gwr to print and plot results
%---------------------------------------------------
% NOTES: uses auxiliary function scoref for cross-validation
%---------------------------------------------------

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin == 7 % user options
 if ~isstruct(info)
    error('bgwrv: must supply the option argument as a structure variable');
 else
 fields = fieldnames(info);
 nf = length(fields);
 % set defaults
 [n k] = size(x);
 bwidth = 0; dtype = 0; q = 0; qmin = k+2; qmax = 5*k;  rval = 4;
 bmin = 0.1; bmax = 20;  % default values for CV gaussian and exponential search

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
    elseif strcmp(fields{i},'rval');
        rval = info.rval;
    end; 
  end; % end of for i
 end; % end of if else

elseif nargin == 7
 bwidth = 0; dtype = 0; dstring = 'gaussian'; rval = 4;
else
 error('Wrong # of arguments to bgwrv');
end;


% error checking on inputs
[nobs nvar] = size(x);
[nobs2 junk] = size(y);
[nobs3 junk] = size(north);
[nobs4 junk] = size(east);

results.ycoord = north;
results.xcoord = east;

if nobs ~= nobs2
 error('bgwrv: y and x must contain same # obs');
elseif nobs3 ~= nobs
 error('bgwrv: north coordinates must equal # obs');
elseif nobs3 ~= nobs4
 error('bgwrv: east coordinates must equal # in north');
end;

switch dtype

case{0,1} % bandwidth cross-validation
if bwidth == 0 % cross-validation

options = optimset('fminbnd');
optimset('MaxIter',500);


if dtype == 0     % Gaussian weights
[bdwt,junk,exitflag,output] = fminbnd('scoref',bmin,bmax,options,y,x,east,north,dtype);
results.bwidth = sqrt(bdwt);
elseif dtype == 1 % exponential weights
[bdwt,junk,exitflag,output] = fminbnd('scoref',bmin,bmax,options,y,x,east,north,dtype);
results.bwidth = sqrt(bdwt);
end;        
 if output.iterations == 500
 fprintf(1,'bgwrv: cv convergence not obtained in %4d iterations',output.iterations);
 else
 results.iter = output.iterations;
 end;

else
 bdwt = bwidth*bwidth; % user supplied bandwidth
 results.bwidth = bwidth;
end;

case{2} % q-nearest neigbhor cross-validation
if q == 0 % cross-validation
q = scoreq(qmin,qmax,y,x,east,north);
results.q = q;
else
% use user-supplied q-value
end;

otherwise

end;

% generate big distance matrix
dmat = zeros(nobs,nobs);
    for j=1:nobs;
        % generate d using GWR distances
        easti = east(j,1);
        northi = north(j,1);
        dx = east - easti;
        dy = north - northi;
        d = dx.*dx + dy.*dy;    
        dmat(:,j) = d;
    end;
    
% generate distance decay matrix
wt = zeros(nobs,nobs);  
if dtype == 1,     % exponential weights
        wt = exp(-dmat/bdwt); 
elseif dtype == 0, % gaussian weights  
        sd = std(sqrt(dmat));
        tmp = matdiv(sqrt(dmat),sd*bdwt);
        wt = stdn_pdf(tmp);
elseif dtype == 2  
% case of tricube weights handled a bit differently
   % sort distance to find q nearest neighbors
 ds = sort(dmat); dmax = ds(q+1,:);
        for j=1:nobs;
 nzip = find(dmat(:,j) <= dmax(1,j));
        wt(nzip,j) = (1-(dmat(nzip,j)/dmax(1,j)).^3).^3;
        end; % end of j-loop
end;  % end of if-else  

wt = sqrt(wt);

% storage for estimates
bsave = zeros(ndraw-nomit,nobs,nvar);
smean = zeros(nobs,1);
vmean = ones(nobs,nobs);
prior.rval = rval;

hwait = waitbar(0,'Gibbs sampling ...');
t0 = clock; 
for i=1:nobs
 nzip = find(wt(:,i) > 0.01);
 ys = y(nzip,1).*wt(nzip,i); xs = matmul(x(nzip,:),wt(nzip,i));
 res = gwr_g(ys,xs,ndraw,nomit,prior);  
 bsave(:,i,:) = res.bdraw;
 vmean(i,nzip) = vmean(i,nzip) + res.vmean';
 smean(i,1) = mean(res.sdraw);
 waitbar(i/nobs);
end; % end loop over nobs
close(hwait);

gtime = etime(clock,t0);

vout = mean(vmean); 

results.bdraw = bsave;
results.meth = 'bgwrv';
results.time = gtime;
results.smean = smean;
results.vmean = vout';
results.nobs = nobs;
results.nvar = nvar;
results.ndraw = ndraw;
results.nomit = nomit;
if dtype == 0
results.dtype = 'gaussian';
elseif dtype == 1
results.dtype = 'exponential';
else
results.dtype = 'tricube';
end;
results.xcoord = east;
results.ycoord = north;
results.r = rval;
results.y = y;
results.x = x;

