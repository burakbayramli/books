function results = bgwr(y,x,east,north,ndraw,nomit,prior);
% PURPOSE: compute Bayesian geographically weighted regression
%          model: y = Xb(i) + e,      e = N(0,sige*V), 
%                 b(i) = f[b(j)] + u, u = delta*sige*inv(x'x)
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          delta = gamma(s,t), 
%          f[b(j)] = b(i-1) for concentric city prior
%          f[b(j)] = W(i) b for contiguity prior
%          f[b(j)] = [exp(-d/b)/sum(exp(-d/b)] b for distance prior
%----------------------------------------------------
% USAGE: results = bgwr(y,x,xcoord,ycoord,ndraw,nomit,prior)
% where: y = dependent variable vector
%        x = explanatory variable matrix
%        xcoord = x-coordinates in space
%        ycoord = y-coordinates in space
%        prior = a structure variable with fields:
%        prior.rval,   improper r value, default=4
%        prior.m,      informative Gamma(m,k) prior on r
%        prior.k,      informative Gamma(m,k) prior on r
%        prior.delta,  improper delta value (default=diffuse)
%        prior.dscale, scalar for delta (with diffuse prior) 
%        prior.s,      informative Gamma(s,t) prior on delta 
%        prior.t,      informative Gamma(s,t) prior on delta
%        prior.ptype, 'concentric' for concentric city smoothing 
%                     'distance'   for distance based smoothing (default)
%                     'contiguity' for contiguity smoothing
%                     'casetti'    for casetti smoothing (not implemented)  
%        prior.ctr, observation # of central point (for concentric prior)
%        prior.W,   (optional) prior weight matrix (for contiguity prior)
%        prior.bwidth = scalar bandwidth to use, or zero 
%                       for cross-validation estimation (default)
%        prior.bmin   = minimum bandwidth to use in CV search
%        prior.bmax   = maximum bandwidth to use in CV search
%                       defaults: bmin = 0.1, bmax = 20                            
%        prior.dtype  = 'gaussian'    for Gaussian weighting 
%                     = 'exponential' for exponential weighting (default)
%                     = 'tricube'     for tri-cube weighting
%        prior.q      = q-nearest neighbors to use for tri-cube weights
%                       (default: CV estimated)  
%        prior.qmin   = minimum # of neighbors to use in CV search
%        prior.qmax   = maximum # of neighbors to use in CV search
%                       defaults: qmin = nvar+2, qmax = 5*nvar          
%        ndraw = # of draws
%        nomit = # of initial draws omitted for burn-in
% ---------------------------------------------------
% RETURNS: a results structure
%        results.meth   = 'bgwr'
%        results.bdraw  = beta draws (ndraw-nomit x nobs x nvar) (a 3-d matrix)
%        results.smean  = mean of sige draws (nobs x 1)
%        results.vmean  = mean of vi draws (nobs x 1)
%        results.lpost  = mean of log posterior (nobs x 1)
%        results.rdraw  = r-value draws (ndraw-nomit x 1)
%        results.ddraw  = delta draws (if diffuse prior used)
%        results.r      = value of hyperparameter r (if input)
%        results.d      = value of hyperparameter delta (if input)
%        results.m      = m prior parameter (if input)
%        results.k      = k prior parameter (if input) 
%        results.s      = s prior parameter (if input)
%        results.t      = t prior parameter (if input)         
%        results.nobs   = nobs
%        results.nvar   = nvars
%        results.ptype  = input string for parameter smoothing relation 
%        results.bwidth= bandwidth if gaussian or exponential
%        results.q     = q nearest neighbors if tri-cube
%        results.dtype = input string for Gaussian,exponential,tricube weights
%        results.iter  = # of simplex iterations for cv
%        results.y     = y data vector
%        results.x     = x data matrix        
%        results.xcoord = x-coordinates
%        results.ycoord = y-coordinates
%        results.ctr    = central point observation # (if concentric prior)
%        results.dist   = distance vector (if ptype = 0)
%        results.time   = time taken for sampling
%---------------------------------------------------
% See also: prt, plt, to print and plot results
%---------------------------------------------------
% References: LeSage, J.P. ``A Family of Geographically Weighted Regression Models'' 
%---------------------------------------------------
% NOTES: - use either improper prior.rval 
%          or informative Gamma prior.m, prior.k, not both of them
%        - for large samples tricube is fastest 
%---------------------------------------------------

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% set defaults
rflag = 0; % =1 for improper user input, =2 for gamma(mm,kk) prior
sflag = 0; % =1 for scaling of delta
mm = 0; kk = 0; 
dflag = 0; % =1 for improper user input, =2 for gamma(mm,kk) prior
ss = 0; tt = 0; 
ptype = 1; % default parameter smoothing prior
ctr = 0;   % central observation for concentric
pstring = 'distance'; % default parameter smoothing prior
wflag = 0; % =1 for user contiguity matrix input
rval = 4;  % heteroscedastic prior
dtype = 1; % exponential weighting
bwidth = 0;% cross-validation estimate of band width
nu=0; d0=0;% diffuse prior on sige
dscale = 1; % default for delta scalar
q = 0;      % to produce tri-cube cross-validation estimates
[nobs k] = size(x);
qmin = k+2; qmax = 5*k; % default values for CV tricube search
bmin = 0.1; bmax = 20;  % default values for CV gaussian and exponential search

if nargin == 7 % user options
 if ~isstruct(prior)
    error('bgwr: must supply the prior information as a structure variable');
 end;
 fields = fieldnames(prior);
 nf = length(fields);
  for i=1:nf
    if strcmp(fields{i},'rval')
        rval = prior.rval; 
        rflag = 1;
    elseif strcmp(fields{i},'ptype')
        pstring = prior.ptype;
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        rflag = 2;
    elseif strcmp(fields{i},'k')
        kk = prior.k;
        rflag = 2;
    elseif strcmp(fields{i},'s')
        ss = prior.s;
        dflag = 1;
    elseif strcmp(fields{i},'t')
        tt = prior.t;
        dflag = 1;
    elseif strcmp(fields{i},'ctr')
        ctr = prior.ctr;
    elseif strcmp(fields{i},'delta')
        delta = prior.delta;
        dflag = 2;
    elseif strcmp(fields{i},'W')
        W = prior.W;
        wflag = 1;    
    elseif strcmp(fields{i},'bwidth')
        bwidth = prior.bwidth; 
    elseif strcmp(fields{i},'dtype')
        dstring = prior.dtype;
       if strcmp(dstring,'gaussian')
        dtype = 0;
       elseif strcmp(dstring,'exponential')
        dtype = 1;
       elseif strcmp(dstring,'tricube')
        dtype = 2;
       end;
    elseif strcmp(fields{i},'q')
       q = prior.q;
    elseif strcmp(fields{i},'qmax');
        qmax = prior.qmax;
    elseif strcmp(fields{i},'qmin');
        qmin = prior.qmin;   
    elseif strcmp(fields{i},'bmin');
        bmin = prior.bmin;   
    elseif strcmp(fields{i},'bmax');
        bmax = prior.bmax;   
    elseif strcmp(fields{i},'dscale');
        dscale = prior.dscale;   
    end;
  end; % end of for i

elseif nargin == 6
 % use defaults
else
 error('Wrong # of arguments to bgwr');
end;

if strcmp(pstring,'concentric')
ptype = 0;
elseif strcmp(pstring,'distance')
ptype = 1;
elseif strcmp(pstring,'contiguity')
ptype = 2;
elseif strcmp(pstring,'casetti')
error('bgwr: casetti smoothing not implemented yet');
end;

[nobs nvar] = size(x);

switch dtype % weighting method

case{0,1} % bandwidth cross-validation
if bwidth == 0 % cross-validation
options = optimset('fminbnd');
optimset('MaxIter',500);

if dtype == 0 % Gaussian
 [bdwt,junk,exitflag,output] = fminbnd('scoref',bmin,bmax,options,y,x,east,north,dtype);
% get GWR estimates as starting values using the bandwidth
info.dtype = 'gaussian'; info.bwidth = sqrt(bdwt);
res = gwr(y,x,east,north,info);
gam = res.beta;
sigi = res.sige;
bwidth = bdwt;
results.bwidth = sqrt(bdwt);
 
elseif dtype == 1 % exponential
 [bdwt,junk,exitflag,output] = fminbnd('scoref',bmin,bmax,options,y,x,east,north,dtype);
% get GWR estimates as starting values using the bandwidth
info.dtype = 'exponential'; info.bwidth = sqrt(bdwt);
res = gwr(y,x,east,north,info);
gam = res.beta;
sigi = res.sige;
bwidth = bdwt;
results.bwidth = sqrt(bdwt);

end;  % end of if else dtype 

 if output.iterations == 500, 
 fprintf(1,'bgwr: cv convergence not obtained in %4d iterations',output.iterations);
 else
 results.iter = output.iterations;
 end;

else % user-supplied bandwidth
 if dtype == 0
 info.dtype = 'gaussian';
 info.bwidth = bwidth;
% get GWR estimates as starting values using user's bandwidth
 res = gwr(y,x,east,north,info);
 elseif dtype == 1
 info.dtype = 'exponential';
 info.bwidth = bwidth;
 res = gwr(y,x,east,north,info); 
end; 

gam = res.beta;
sigi = res.sige;
results.bwidth = bwidth;
bwidth = bwidth*bwidth; % user supplied bandwidth
 
end;

case{2} % q-nearest neigbhor cross-validation

if q == 0 % cross-validation
q = scoreq(qmin,qmax,y,x,east,north);
results.q = q;
info.dtype = 'tricube'; info.q = q;
res = gwr(y,x,east,north,info);
gam = res.beta;
sigi = res.sige;

else
% use user-supplied q-value
results.q = q;
info.dtype = 'tricube'; info.q = q;
res = gwr(y,x,east,north,info);
gam = res.beta;
sigi = res.sige; 
end;

otherwise

end; % end of switch on dtype for weighting method

switch ptype % switch on prior parameter smoothing method
             % big switch goes to end of file

case {0} % concentric city prior
if ctr == 0
        error('bgwr: no central observation # supplied');
end;

% compute distance from central city
xctr = east(ctr,1);
yctr = north(ctr,1);
xord = (east - xctr);
yord = (north - yctr);
dist = sqrt(xord.*xord+yord.*yord);
% sort distance from central city
[dsort, di] = sort(dist);

% sort the data by distance from central observation
ys = y(di,1); xs = x(di,:);

% sort GWR initial values by distance
gams = gam(di,:); sigis = sigi(di,1);
gam1 = gams(1,:)'; % we only need the 1st observation here
clear gam;
clear gams;
% sort x-y coordinates by distance
norths = north(di,1); easts = east(di,1);

% generate big distance matrix for all observations
dmat = zeros(nobs,nobs);
    for j=1:nobs;
        % generate d using GWR distances
        easti = easts(j,1);
        northi = norths(j,1);
        dx = easts - easti;
        dy = norths - northi;
        d = dx.*dx + dy.*dy;    
        dmat(:,j) = d;  
    end;

% generate distance decay matrix
wt = zeros(nobs,nobs);  
if dtype == 1,     % exponential weights
        wt = exp(-dmat/bwidth); 
elseif dtype == 0, % gaussian weights  
        sd = std(sqrt(dmat));
        tmp = matdiv(sqrt(dmat),sd*bwidth);
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

% take the sqrt once      
wt = sqrt(wt);

% storage for estimates
bsave = zeros(ndraw-nomit,nobs,nvar);
smean = zeros(nobs,1);
vmean = ones(nobs,nobs);

if (dflag == 0 | dflag == 1)
dsave = zeros(ndraw-nomit,1);
end;
if rflag == 2
rsave = zeros(ndraw-nomit,1);
end;
vsave = zeros(1,nobs);
lpost = zeros(nobs,1);
dtemp = zeros(nomit,1);

in = ones(nobs,1);
if dflag == 0, % we need an initial delta value
 delta = 1; 
end;

if dscale ~= 1,
deltas = dscale; % save original dscale
dscale = 1;
sflag = 1;
end;

t0 = clock; 
hwait = waitbar(0,'Gibbs sampling ...');

for iter = 1:ndraw;
 bsum = 0.0;
 
for i = 1:nobs; % loop over all observations
  sige = sigis(i,1);
  sigd = sige*delta; 
% set up prior restriction 
        if i > 1    
        bim1 = bi;
        else
        bim1 = gam1; % use GWR for obs 1
        end; 
% create y-tilde, x-tilde
  nzip = find(wt(:,i) >= 0.01); 
  V = sqrt(vmean(nzip,i));
  xt = matmul(wt(nzip,i),xs(nzip,:));
  yt = wt(nzip,i).*ys(nzip,1);
  yss = wt(nzip,i).*V.*ys(nzip,1);
  xss = matmul(V,xt);
  xpx = xt'*xt;
  xx = inv(xpx);
% concentric city prior 
  Q = xpx/sigd;
  Qpc = Q*bim1;  
% update bi estimates
  xpxi = inv(xss'*xss + sige*Q); 
  xpy = (xss'*yss + sige*Qpc); 
  bi = xpxi*xpy;    
  a = chol(xpxi);
  bi = sqrt(sige)*a'*randn(nvar,1) + bi; 
% update sige 
  nu1 = length(nzip) + nu; 
  e = yss - xss*bi;
  d1 = d0 + e'*e;
  chi = chis_rnd(1,nu1);
  sige = d1/chi; sigis(i,1) = sige;
  sigd = sige*delta;  
% update vi
  e = yt - xt*bi;
  chiv = chis_rnd(length(nzip),rval+1);   
  vi = ((e.*e./sige) + rval)./chiv;
  vmean(nzip,i) = ones(length(nzip),1)./vi;   
% compute bsum for delta update below
  if dflag == 0
  bsum = bsum + ((bi-bim1)'*xx*(bi-bim1))/sigd; 
  end;
% store results in unsorted order
  if iter > nomit    
  bsave(iter-nomit,di(i,1),:) = bi';
  smean(di(i,1),1) = smean(di(i,1),1) + sige;
% compute log posterior
  tvec = norm_pdf((y-x*bi)./(sige*vmean(:,i)));
  tind = find(tvec > 0); % avoid log of zero
  esum = sum(wt(tind,i).*(log(tvec(tind,1)) - log(sige*vmean(tind,i))));
  lpost(i,1) = lpost(i,1) + esum;
  end; 

end; % end loop over observations

% update delta
 if dflag == 0
% get delta draw
      chi = chis_rnd(1,nobs*nvar);
      delta = dscale*(bsum/chi);  
      dtemp(iter,1) = delta;       
 elseif dflag == 1
      delta = gamm_rnd(1,1,ss,tt); 
% note case of dflag == 2, keep same delta value during sampling
 end;

if iter == nomit, % check for case of scaling delta
 if sflag == 1,   % case of scaling delta
 delta = mean(dtemp);
 dflag = 2;       % turn off updating delta
 delta = deltas*delta; % apply user's dscale
 end;
end;

% save delta and compute mean vi to save
if iter > nomit
vsave = vsave + in'./mean(vmean');
dsave(iter-nomit,1) = delta;
end;

    waitbar(iter/ndraw)

% [iter sige delta max(mean(vmean)) ]  

end; % end of loop over draws
close(hwait); 

gtime = etime(clock,t0);
vout = vsave/(ndraw-nomit);
% unsort vi estimates
vm = unsort(vout',di);
lpost = lpost/(ndraw-nomit);
smean = smean/(ndraw-nomit);

% ------- return results
results.bdraw = bsave;
results.meth = 'bgwr';
results.time = gtime;
results.smean = smean;
results.vmean = vm;
results.lpost = lpost;
results.nobs = nobs;
results.nvar = nvar;
results.ndraw = ndraw;
results.nomit = nomit;
results.ptype = pstring;
results.dtype = info.dtype;
if dtype == 0
 results.bwidth = sqrt(bwidth);
elseif dtype == 1
 results.bwidth = sqrt(bwidth);
elseif dtype == 2
 results.q = q;
end;
results.ctr = ctr;
results.y = y;
results.x = x;
results.m = mm;
results.k = kk;
results.xcoord = east;
results.ycoord = north;
if rflag == 0
results.r = rval;
elseif rflag == 1
results.r = rval;        
elseif rflag == 2,
results.r = mean(rsave);
end;
results.s = ss;
results.t = tt;
if dflag == 2
results.d = delta;
elseif dflag == 0
results.d = mean(dsave);
results.ddraw = dsave;        
elseif dflag == 1
results.d = mean(dsave);
results.ddraw = dsave;        
end;

case {1} % distance prior
 
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
        wt = exp(-dmat/bwidth); 
elseif dtype == 0, % gaussian weights  
        sd = std(sqrt(dmat));
        tmp = matdiv(sqrt(dmat),sd*bwidth);
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

% normalize row-sums to unity
% and set weight for own-obs to zero
wtn = zeros(nobs,nobs);
    for j=1:nobs
 wtmp = wt(j,:); wtmp(1,j) = 0;
        wsum = sum(wtmp);
        wtn(j,:) = wtmp/wsum;
    end;

% storage for estimates
bsave = zeros(ndraw-nomit,nobs,nvar);
smean = zeros(nobs,1);
if (dflag == 0 | dflag == 1)
dsave = zeros(ndraw-nomit,1);
end;
if rflag == 2
rsave = zeros(ndraw-nomit,1);
end;
vmean = ones(nobs,nobs);
vsave = zeros(1,nobs);
lpost = zeros(nobs,1);
dtemp = zeros(nomit,1);

in = ones(nobs,1);
JKg = zeros(nvar,1);

if dflag == 0, % we need an initial delta value
 delta = 1; 
end;

if dscale ~= 1,
deltas = dscale; % save original dscale
sflag = 1;
dscale = 1;
end;


t0 = clock; 
hwait = waitbar(0,'Gibbs sampling ...');
for iter = 1:ndraw;
 bsum = 0.0;
 
 for i = 1:nobs; % loop over all observations    
 sige = sigi(i,1);
 sigd = sige*delta;
 nzip = find(wt(:,i) >= 0.01);
 V = sqrt(vmean(nzip,i));
  yt = wt(:,i).*y;
  xt = matmul(wt(:,i),x);
  ys = yt(nzip,1).*V;
  xs = matmul(V,xt(nzip,:));
  xpx = xt(nzip,:)'*xt(nzip,:);
  xx = inv(xpx);
% distance smoothing prior
for j=1:nvar
  JKg(j,1) = wtn(i,:)*gam(:,j);
end;
  Q = xpx/sigd;
  Qpc = Q*JKg; 
% update b 
  xpxi = inv(xs'*xs + sige*Q); 
  xpy = (xs'*ys + sige*Qpc); 
  bi = xpxi*xpy;    
  a = chol(xpxi);
  bi = sqrt(sige)*a'*randn(nvar,1) + bi;          
% update gamma for next trip
  gam(i,:) = bi';
% update sige 
  nu1 = length(nzip) + nu; 
  e = ys - xs*bi;
  d1 = d0 + e'*e;
  chi = chis_rnd(1,nu1);
  sige = d1/chi; 
  sigi(i,1) = sige;
  sigd = sige*delta;  
% update vi
  e = yt(nzip,1) - xt(nzip,:)*bi;
  chiv = chis_rnd(length(nzip),rval+1);   
  vi = ((e.*e./sige) + rval)./chiv;
  vmean(nzip,i) = ones(length(nzip),1)./vi;   
% compute bsum for delta update below
if dflag == 0
  bsum = bsum + ((bi-JKg)'*xx*(bi-JKg))/sigd; 
end;
  if iter > nomit % save draws
  bsave(iter-nomit,i,:) = bi;
  smean(i,1) = smean(i,1) + sigi(i,1);
% compute log posterior
  tvec = norm_pdf((y-x*bi)./(sige*vmean(:,i)));
  tind = find(tvec > 0); % avoid log of zero
  esum = sum(wt(tind,i).*(log(tvec(tind,1)) - log(sige*vmean(tind,i))));
  lpost(i,1) = lpost(i,1) + esum;
  end; 
end; % end loop over observations

% update delta
if dflag == 0
% get delta draw
      chi = chis_rnd(1,nobs*nvar);
      delta = dscale*(bsum/chi);
      dtemp(iter,1) = delta;         
elseif dflag == 1
      delta = gamm_rnd(1,1,ss,tt); 
% note case of dflag == 2, keep same delta value during sampling
end;

if iter == nomit, % check for case of scaling delta
 if sflag == 1,   % case of scaling delta
 delta = mean(dtemp);
 dflag = 2;       % turn off updating delta
 delta = deltas*delta; % apply user's dscale
 end;
end;

if iter > nomit
vsave = vsave + in'./mean(vmean');
dsave(iter-nomit,1) = delta;
end;

    waitbar(iter/ndraw)

%[iter sige delta max(mean(vmean')) ]  

end; % end of loop over draws
close(hwait);

gtime = etime(clock,t0);
vsave = vsave/(ndraw-nomit);
lpost = lpost/(ndraw-nomit);
smean = smean/(ndraw-nomit);

results.bdraw = bsave;
results.meth = 'bgwr';
results.time = gtime;
results.smean = smean;
results.vmean = vsave';
results.lpost = lpost;
results.nobs = nobs;
results.nvar = nvar;
results.ndraw = ndraw;
results.nomit = nomit;
results.ptype = pstring;
results.dtype = info.dtype;
results.xcoord = east;
results.ycoord = north;
results.y = y;
results.x = x;
results.m = mm;
results.k = kk;
if rflag == 0
results.r = rval;
elseif rflag == 1
results.r = rval;  
elseif rflag == 2,
results.r = mean(rsave);
end;
results.s = ss;
results.t = tt;
if dflag == 2
results.d = delta;
elseif dflag == 0
results.d = mean(dsave);
results.ddraw = dsave;        
elseif dflag == 1
results.d = mean(dsave);
results.ddraw = dsave;        
end;


case {2} % contiguity  prior

if wflag == 0
% generate contiguity matrix using x-y coordinates
[junk W junk2]= xy2cont(east,north);
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
        wt = exp(-dmat/bwidth); 
elseif dtype == 0, % gaussian weights  
        sd = std(sqrt(dmat));
        tmp = matdiv(sqrt(dmat),sd*bwidth);
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

bsave = zeros(ndraw-nomit,nobs,nvar);
smean = zeros(nobs,1);
if (dflag == 0 | dflag == 1)
dsave = zeros(ndraw-nomit,1);
end;
if rflag == 2
rsave = zeros(ndraw-nomit,1);
end;
vmean = ones(nobs,nobs);
lpost = zeros(nobs,1);
vsave = zeros(1,nobs);
dtemp = zeros(nomit,1);

in = ones(nobs,1);
JKg = zeros(nvar,1);

if dflag == 0, % we need an initial delta value
 delta = 1; 
end;

if dscale ~= 1,
deltas = dscale; % save original dscale
sflag = 1;
scale = 1;
end;

t0 = clock; 
hwait = waitbar(0,'Gibbs sampling ...');

for iter = 1:ndraw;
 bsum = 0.0;

 for i = 1:nobs; % loop over all observations
 
 sige = sigi(i,1);
 sigd = sige*delta;
 nzip = find(wt(:,i) >= 0.01);
 V = sqrt(vmean(nzip,i));
  xt = matmul(wt(:,i),x);
  yt = y.*wt(:,i);
  ys = yt(nzip,1).*V;
  xs = matmul(V,xt(nzip,:));
  xpx = xt(nzip,:)'*xt(nzip,:);
  xx = inv(xpx);
% contiguity prior
  for j=1:nvar
  JKg(j,1) = W(i,:)*gam(:,j);
  end;  
  Q = xpx/sigd;
  Qpc = Q*JKg; 
% update b 
  xpxi = inv(xs'*xs + sige*Q); 
  xpy = (xs'*ys + sige*Qpc); 
  bi = xpxi*xpy;    
  a = chol(xpxi);
  bi = sqrt(sige)*a'*randn(nvar,1) + bi;          
% update gamma for next trip
  gam(i,:) = bi';
% update sige 
  nu1 = length(nzip) + nu; 
  e = ys - xs*bi;
  d1 = d0 + e'*e;
  chi = chis_rnd(1,nu1);
  sige = d1/chi; sigi(i,1) = sige;
  sigd = sige*delta;
% update vi
  e = yt(nzip,1) - xt(nzip,:)*bi;
  chiv = chis_rnd(length(nzip),rval+1);   
  vi = ((e.*e./sige) + rval)./chiv;
  vmean(nzip,i) = ones(length(nzip),1)./vi;   
% compute bsum for delta update below
if dflag == 0
  bsum = bsum + ((bi-JKg)'*xx*(bi-JKg))/sigd; 
end;
  if iter > nomit % save draws
  bsave(iter-nomit,i,:) = bi';
  smean(i,1) = smean(i,1) + sige;
% compute log posterior
  tvec = norm_pdf((y-x*bi)./(sige.*vmean(:,i)));
  tind = find(tvec > 0); % avoid log of zero
  esum = sum(wt(tind,i).*(log(tvec(tind,1)) - log(sige*vmean(tind,i))));
  lpost(i,1) = lpost(i,1) + esum;
  end;
end; % end loop over observations

% update delta
 if dflag == 0
% get delta draw
      chi = chis_rnd(1,nobs*nvar);
      delta = dscale*(bsum/chi);
      dtemp(iter,1) = delta;         
 elseif dflag == 1
      delta = gamm_rnd(1,1,ss,tt); 
% note case of dflag == 2, keep same delta value during sampling
end;

if iter == nomit, % check for case of scaling delta
 if sflag == 1,   % case of scaling delta
 delta = mean(dtemp);
 dflag = 2;       % turn off updating delta
 delta = deltas*delta; % apply user's dscale
 end;
end;

if iter > nomit
vsave = vsave + in'./mean(vmean');
dsave(iter-nomit,1) = delta;
end;

    waitbar(iter/ndraw)

end; % end of loop over draws
close(hwait);

gtime = etime(clock,t0);
vsave = vsave/(ndraw-nomit);
lpost = lpost/(ndraw-nomit);
smean = smean/(ndraw-nomit);

results.bdraw = bsave;
results.meth = 'bgwr';
results.time = gtime;
results.smean = smean;
results.vmean = vsave';
results.lpost = lpost;
results.nobs = nobs;
results.nvar = nvar;
results.ndraw = ndraw;
results.nomit = nomit;
results.ptype = pstring;
results.dtype = info.dtype;
results.xcoord = east;
results.ycoord = north;
results.y = y;
results.x = x;
results.m = mm;
results.k = kk;
if rflag == 0
results.r = rval;
elseif rflag == 1
results.r = rval;  
elseif rflag == 2,
results.r = mean(rsave);
end;
results.s = ss;
results.t = tt;
if dflag == 2
results.d = delta;
elseif dflag == 0
results.ddraw = dsave;        
results.d = mean(dsave);
elseif dflag == 1
results.d = mean(dsave);
results.ddraw = dsave;        
end;

 
otherwise
        error('bgwr: prior parameter smoothing relation unkown to bgwr');
end;
