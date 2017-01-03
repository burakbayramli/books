function results = sar_gcbma(y,x,W,ndraw,prior)
% PURPOSE: MC^3 x-matrix specification for homoscedastic SAR model
%          where the model contains an intercept
% ------------------------------------------------------
% usage: results = sar_gcbma(y,x,W,ndraw,prior)
% where: % where: y = dependent variable vector (nobs x 1)
%        x = studentized or well-scaled x-matrix with no intercept 
%        W = spatial weight matrix (standardized, row-sums = 1)
%    ndraw = # of draws
%    prior = a structure variable with:
%            prior.g     = g-value for g-prior, default: max(1/n,1/k^2)
%            prior.nmodels = # of top models to save (default = 1000)
%            prior.nu    = informative Gamma(nu,d0) prior on sige
%            prior.d0    = default: nu=0,d0=0 (diffuse prior)
%            prior.a1    = parameter for beta(a1,a2) prior on rho (default = 1.01)
%            prior.a2    = (default = 1.01) see: 'help beta_prior'
%            prior.eig   = 0 for default rmin = -1,rmax = +1, 1 for eigenvalue calculation of these
%            prior.rmin  = (optional) min rho used in sampling (default = -1)
%            prior.rmax  = (optional) max rho used in sampling (default = 1)  
%            prior.lflag = 0 for full lndet computation (default = 1, fastest)
%                        = 1 for MC approx (fast for large problems)
%                        = 2 for Spline approx (medium speed)
%            prior.order = order to use with prior.lflag = 1 option (default = 50)
%            prior.iter  = iters to use with prior.lflag = 1 option (default = 30) 
%            prior.lndet = a matrix returned by sar, sar_g, sarp_g, etc.
%                          containing log-determinant information to save time
%-------------------------------------------------------------
% RETURNS:  a structure:
%          results.meth    = 'sar_gcbma'
%          results.g       = g-value for prior (from input, or default value)
%          results.munique = # of unique models found
%          results.nmodels = # of top models to save from input or (default = 1000)
%          results.freq    = a matrix with frequency distribution of
%                            variables appearing in all unique models found
%          results.models  = a matrix with only information for top nmodels
%          results.allmodels = a matrix with information on all unique models found
%          results.mprob   = a vector of posterior model probabilities based on all unique models
%          results.vprob   = a vector of variable probabilities based on all unique models
%          results.nobs    = # of observations
%          results.nvar    = # of variables in x-matrix
%          results.ndraw   = # of draws
%          results.y       = y-vector from input (nobs x 1)
%          results.nu      = nu prior parameter
%          results.d0      = d0 prior parameter
%          results.a1      = a1 parameter for beta prior on rho from input, or default value
%          results.a2      = a2 parameter for beta prior on rho from input, or default value
%          results.time1   = time for eigenvalue calculation
%          results.time2   = time for log determinant calcluation
%          results.time3   = time for BMA sampling
%          results.time    = total time taken  
%          results.rmax    = 1/max eigenvalue of W (or rmax if input)
%          results.rmin    = 1/min eigenvalue of W (or rmin if input)          
%          results.lflag   = lflag from input
%          results.iter    = prior.iter option from input
%          results.order   = prior.order option from input
%          results.limit   = matrix of [rho lower95,logdet approx, upper95] 
%                           intervals for the case of lflag = 1
%          results.dflag   = dflag value from input (or default value used)
%          results.lndet  = a matrix containing log-determinant information
%                           (for use in later function calls to save time)
% --------------------------------------------------------------
% NOTES: 1) the log-marginals for the top 1000 models are written to a file lmarginal.sar
%        as vectors over a grid of rho values for later use 
%        2) the model descriptions for the top 1000 models are written to a file models.sar
% --------------------------------------------------------------
% SEE ALSO: prt_bmas(), for printing results
%           model_averages() function, that can be used to construct
%           estimates based on averaging over models using posterior
%           model probabilities as weights
% --------------------------------------------------------------
% REFERENCES: James P. LeSage and Olivier Parent, 
% `Bayesian Model Averaging for Spatial Econometric Models', working paper 
%
% Fernandez,Carmen, Eduardo Ley, and Mark F. J. Steel, (2001a)
%'Model uncertainty in cross-country growth regressions,'
% Journal of Applied Econometrics}, Volume 16, number 5, pp. 563 - 576.
%
% Fernandez,Carmen, Eduardo Ley, and Mark F. J. Steel, (2001b)
% 'Benchmark priors for Bayesian model averaging', Journal of Econometrics
% Volume 100, number 2, pp. 381-427.
%----------------------------------------------------------------

% written by:
% James P. LeSage, last updated 6/2004
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% NOTE: some of the speed for large problems comes from:
% the use of methods pioneered by Pace and Barry.
% R. Kelley Pace was kind enough to provide functions
% lndetmc, and lndetint from his spatial statistics toolbox
% for which I'm very grateful.


% error checking on inputs
[n junk] = size(y);
[n1 k] = size(x);
[n3 n4] = size(W);
time1 = 0;
time2 = 0;
time3 = 0;
time4 = 0;

nobsa = n;

results.nobs  = n;
results.nvar  = k;
results.y = y;      

if n1 ~= n
error('sar_gcbma: x-matrix contains wrong # of observations');
elseif n3 ~= n4
error('sar_gcbma: W matrix is not square');
elseif n3~= n
error('sar_gcbma: W matrix is not the same size at y,x');
end;

if nargin == 4
    prior.lflag = 1;
end;

[nu,d0,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,a1,a2,g,gmodels] = sar_parse(prior,x);


results.order = order;
results.iter = iter;

timet = clock; % start the timer

[rmin,rmax,time1] = sar_eigs(eflag,W,rmin,rmax,n);
results.time1 = time1;

[detval,time2] = sar_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
results.time2 = time2;
na = length(detval(:,1));
% overide nmodels to speed things up
nmodels = min([gmodels 10]);
gmodels = max([1000 gmodels]);
% storage for draws
          nvar = k;
          maxlog = ones(gmodels,1)*(-Inf);
          vsave = zeros(ndraw,nvar);
          msave = zeros(1,nvar);
          visits = zeros(ndraw,1);
          lsave = zeros(na,1);
          vfreqs = zeros(1,nvar);
          
          modsave = zeros(gmodels,nvar); % storage for definition of 1000 best models 
          logsave = zeros(gmodels,na);

% ====== initializations
% compute this stuff once to save time
in = ones(n,1);
Wy = sparse(W)*y;

% start with a random selection of variables
vtmp=zeros(nvar,1);
for v=1:nvar
    vtmp(v)=bino_rnd(1,0.5,1,1);
end
vin=selif(seqa(1,1,nvar).*vtmp,vtmp)';
vout=delif(seqa(1,1,nvar)-seqa(1,1,nvar).*vtmp,vtmp)'; 

cnt = 0;

%<====================== start draws
t0 = clock;
hwait = waitbar(0,'SAR BMA : MCMC sampling ...');
for i=1:ndraw; 
    
% choose new model
[vinew vonew] = sample(vin,vout);
for j=1:length(vinew);
vsave(i,vinew(1,j)) = 1.0;
end;
vfreqs = vfreqs + vsave(i,:);

[j visits] = find_new(i,vsave,vinew,visits);

if i == 1 % first draw, compute logpost for initial model
lmarg = bmapost(y,x,vinew,W,detval,a1,a2,nu,d0,g);
maxl = max(lmarg);
maxlog(i,1) = maxl;
lsave = [lsave lmarg];
msave = [msave
         vsave(i,:)];
modsave(i,:) =  vsave(i,:);     
logsave(i,:) = lmarg';
cnt = cnt+1; % count of unique models found
  lmarg_old = lmarg;
end; % end of if i == 1


if visits(i,1) ~= 0 % we found a new model
cnt = cnt+1; % increase counter for new models
lmarg_new = bmapost(y,x,vinew,W,detval,a1,a2,nu,d0,g);

  if cnt < gmodels
  lsave = [lsave lmarg_new];
  msave = [msave
           vsave(i,:)];
  end;
maxl = max(lmarg_new);
  if cnt < gmodels
    maxlog(cnt,1) = maxl;
    modsave(cnt,:) = vsave(i,:);
    logsave(cnt,:) = lmarg_new';
  elseif cnt == gmodels
    maxlog(cnt,1) = maxl;
    modsave(cnt,:) = vsave(i,:);
    logsave(cnt,:) = lmarg_new';
    % sort the maxlog vector
    [maxlog,maxi] = sort(maxlog);
    tmp = logsave(maxi,:); % bring along the log-marginals matrix
    logsave = tmp;
    tmp = modsave(maxi,:); % bring along the model definitions
    modsave = tmp;
  else
    ind = find(maxl > maxlog);
    if length(ind) == 0 
    %  we don't save this log-marginal
    else
    % insert this log-marginal vector into the 1000 best models
    % pitch lowest log-marginal
      if length(ind) == 1 % special case where we replace just the lowest
       logsave(1,:) = lmarg_new';
       modsave(1,:) = vsave(i,:); % replace model definitions
      else
      logsave(1:ind(end-1),:) = logsave(2:ind(end),:); % pitch the lowest
      logsave(ind(end),:) = lmarg_new'; % insert the new one
      modsave(1:ind(end-1),:) = modsave(2:ind(end),:); % pitch the lowest model defintiion
      modsave(ind(end),:) = vsave(i,:); % insert the new model definition
      end;
    end;
  end;

else % we have a model that we have already computed the log-marginal for
     % so, we could look it up.
  if cnt < gmodels
  tmps = matsub(vsave(i,:),msave);
  ind = ~any(tmps');
  chk = find(ind == 1);
  lmarg_new = lsave(:,chk(1,1));
  else
% recompute it
  lmarg_new = bmapost(y,x,vinew,W,detval,a1,a2,nu,d0,g);
  end;
end;

bf = model_odds(detval(:,1),lmarg_new,lmarg_old);
 if bf >=1; 
     flag = 1; 
 else 
     flag = bino_rnd(1,bf,1,1); 
 end;

 if flag == 1 % change models
 vin = vinew;
 vout = vonew;
 lmarg_old = lmarg_new;
 end;
 
waitbar(i/ndraw);         
end; % end of sampling loop
close(hwait);

time3 = abs(etime(clock,t0));
results.time3 = time3;

vfreqs = vfreqs/ndraw;

%results.munique = cnt;

% write out results for 1000 best models

tmp =  logsave;
save lmarginal.sar tmp  -ascii;
tmp = modsave;
save models.sar tmp -ascii;

psum = 0.0;
posts = [];
na = length(detval);

load lmarginal.sar;
load models.sar;

[nlog,na] = size(lmarginal);
adj = max(max(lmarginal));
madj = lmarginal - adj;
  tmp = exp(madj);
  % trapezoid rule integration
  for i=1:nlog
  xx = tmp(i,:)';
  yy = detval(:,1).*ones(na,1);
  isum = 0.0;
  isum = sum(diff(yy).*(xx(1:end-1)+xx(2:end))/2);
  % compute posterior probability
  psum = psum + isum;
  posts = [posts
          isum];
  end;

postprob = posts/psum;

[probs_sort,pind] = sort(postprob);
results.mprob = probs_sort;
msort = models(pind,:);
out = [msort probs_sort];
results.allmodels = out; % put out results for all unique models found
[no,junk] = size(out);

% just pull out nmodels requested by the user
if (nmodels > no) % the user wants more models output than were found
    nmodels = no; % we can only output what was found
else
    % the user is okay
end;


results.vprob = vfreqs; % compute variable probabilities based on 
                            % all unique models found

results.models = msort(no-nmodels+1:no,:);

results.ndraw = ndraw;
results.nu = nu;
results.d0 = d0;
results.a1 = a1;
results.a2 = a2;
results.rmax = rmax; 
results.rmin = rmin;
results.lflag = ldetflag;
results.lndet = detval;
results.g = g;



function [nu,d0,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,a1,a2,g,gmodels] = sar_parse(prior,x)
% PURPOSE: parses input arguments for sar_gcbma models
% ---------------------------------------------------
%  USAGE: [nu,d0,rho,sige,rmin,rmax,detval,ldetflag,eflag,mflag,order,iter,a1,a2,g] = 
%                           sar_parse(prior,x)
% where prior contains the structure variable with inputs,
% x is the matrix of explanatory variables input
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% set defaults
[n,k] = size(x);
g1 = (k*k);
g2 = n;
g = 1/max([g1 g2]); % Fernandez, Ley Steel default g-value
gmodels = 1000;

eflag = 0;     % default to not computing eigenvalues
ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
order = 50;    % there are parameters used by the MC det approx
iter = 30;     % defaults based on Pace and Barry recommendation
rmin = -1;     % use -1,1 rho interval as default
rmax = 1;
detval = 0;    % just a flag
rho = 0.5;
sige = 1.0;
nu = 0;
d0 = 0;
a1 = 1.01;
a2 = 1.01;

fields = fieldnames(prior);
nf = length(fields);
if nf > 0
 for i=1:nf
    if strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;  
    elseif strcmp(fields{i},'g')
       g = prior.g; 
    elseif strcmp(fields{i},'nmodels')
       gmodels = prior.nmodels; 
    elseif strcmp(fields{i},'dflag')
       metflag = prior.dflag; 
    elseif strcmp(fields{i},'a1')
       a1 = prior.a1; 
    elseif strcmp(fields{i},'a2')
       a2 = prior.a2; 
    elseif strcmp(fields{i},'rmin')
        rmin = prior.rmin; eflag = 0;
    elseif strcmp(fields{i},'rmax')
        rmax = prior.rmax;  eflag = 0;
    elseif strcmp(fields{i},'lndet')
    detval = prior.lndet;
    ldetflag = -1;
    eflag = 0;
    rmin = detval(1,1);
    nr = length(detval);
    rmax = detval(nr,1);
    elseif strcmp(fields{i},'lflag')
        tst = prior.lflag;
        if tst == 0,
        ldetflag = 0; 
        elseif tst == 1,
        ldetflag = 1; 
        elseif tst == 2,
        ldetflag = 2; 
        else
        error('sar_g: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = prior.order;  
    elseif strcmp(fields{i},'iter')
        iter = prior.iter; 
    elseif strcmp(fields{i},'dflag')
        metflag = prior.dflag;
    elseif strcmp(fields{i},'eig')
        eflag = prior.eig;
    end;
 end;

else, % the user has input a blank info structure
      % so we use the defaults
end; 

function [rmin,rmax,time2] = sar_eigs(eflag,W,rmin,rmax,n);
% PURPOSE: compute the eigenvalues for the weight matrix
% ---------------------------------------------------
%  USAGE: [rmin,rmax,time2] = far_eigs(eflag,W,rmin,rmax,W)
% where eflag is an input flag, W is the weight matrix
%       rmin,rmax may be used as default outputs
% and the outputs are either user-inputs or default values
% ---------------------------------------------------


if eflag == 1 % compute eigenvalues
t0 = clock;
opt.tol = 1e-3; opt.disp = 0;
lambda = eigs(sparse(W),speye(n),1,'SR',opt);  
rmin = 1/real(lambda);   
rmax = 1;
time2 = etime(clock,t0);
else
time2 = 0;
end;


function [detval,time1] = sar_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
% PURPOSE: compute the log determinant |I_n - rho*W|
% using the user-selected (or default) method
% ---------------------------------------------------
%  USAGE: detval = far_lndet(lflag,W,rmin,rmax)
% where eflag,rmin,rmax,W contains input flags 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------


% do lndet approximation calculations if needed
if ldetflag == 0 % no approximation
t0 = clock;    
out = lndetfull(W,rmin,rmax);
time1 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];
    
elseif ldetflag == 1 % use Pace and Barry, 1999 MC approximation

t0 = clock;    
out = lndetmc(order,iter,W,rmin,rmax);
time1 = etime(clock,t0);
results.limit = [out.rho out.lo95 out.lndet out.up95];
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];

elseif ldetflag == 2 % use Pace and Barry, 1998 spline interpolation

t0 = clock;
out = lndetint(W,rmin,rmax);
time1 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];

elseif ldetflag == -1 % the user fed down a detval matrix
    time1 = 0;
        % check to see if this is right
        if detval == 0
            error('sar_g: wrong lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('sar_g: wrong sized lndet input argument');
        elseif n1 == 1
            error('sar_g: wrong sized lndet input argument');
        end;          
end;

function [j,visits] = find_new(i,vsave,vinew,visits)
% PURPOSE: determines if the variables in vinew represent a new model
%          (called by bma_g)
%-------------------------------------------------------
% USAGE: [j,visits] = find_new(i,vsave,vinew,visits)
% where:        i   = size of vsave to search = draw #
%             vsave = (i x nvar) matrix of indicators for models
%             vinew = (1 x nvar) vector with current model indicators
%            visits = (i x nvar) matrix for recording # of visits to a model
%-------------------------------------------------------
% RETURNS:      j = index to an old model found
%          visits = matrix recording visits to old models
%-------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics 7/2003
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[junk nvar] = size(vsave);
vitmp = zeros(1,nvar);
for k=1:nvar
    for j=1:length(vinew)
      if vinew(1,j) == k
        vitmp(1,k) = 1;
      end;
    end;
end;

nobs = i;
new = 0;
j = 1;
while (new == 0 & j <= nobs)
if vsave(j,:) == vitmp(1,:)
visits(j,1) = visits(j,1) + 1;
new = 1;
end;
j = j+1;
end;

j = j-1;

function lmout = bmapost(y,xall,vin,W,detval,a1,a2,nu1,d1,g)
% PURPOSE: evaluates log marginal posterior of SAR bma model
%          (SAR Bayesian model averaging)
%-----------------------------------------------------------
% USAGE: lpost = bmapost(y,xall,vin,W,g)
% where:   y = dependent variable vector (nobs x 1)
%       xall = explanatory variables matrix (nobs x k)
%        vin = a 1xk vector of columns to use from x, e.g. [1 3 5]
%          W = spatial weight matrix
%          g = g prior hyperparameter
%      lndet = a vector with rho, lndet values
%-----------------------------------------------------------
% RETURNS: lpost = log marginal posterior, a vector the length of aval
%                  containing the log marginal over a grid of alpha values
%-----------------------------------------------------------
% REFERENCES: Raftery and Madigan (1997) 'Bayesian model averaging
% for linear regression models', 92, pp. 179-191
% ----------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics 7/2003
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


[nobs,nv1] = size(xall);
nvar = length(vin);
v1 = nvar;
for i=1:nvar
    if vin(1,i) >= nv1
        v1 = i; % # of variables in x1
    end;
end;    

xt = [ones(nobs,1) xall(:,vin)];
[nobs,k] = size(xt);
iota = ones(nobs,1);
Wy = sparse(W)*y;

% now compute vectorized log-marginal likelihood
% to use in trapezoid numerical integration over rho
          xpx = xt'*xt;
          b0t = (xpx)\(xt'*y);
          bdt = (xpx)\(xt'*Wy);
          e0 = y - xt*b0t;
          ed = Wy - xt*bdt;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
          
          Qvec = zeros(length(detval(:,1)),1);
          for i=1:length(detval(:,1));
              ybar = mean(y) - detval(i,1)*mean(Wy);
              yhat = y - detval(i,1)*Wy - ybar*iota;
              Qvec(i,1) = (g/(1+g))*(yhat'*yhat);
          end;

% evaluate vectorized log-marginal likelihood
lmout = sar_marginal(detval,epe0,eped,epe0d,nobs,k,a1,a2,nu1,d1,g,Qvec);


function odds = model_odds(rho_vec,lmarg1,lmarg2)
% PURPOSE: computes Bayes factor for 2 models using vectorized log-marginals as input
% ---------------------------------------------------
%  USAGE: probs = model_odds(rho_vec,logmarginal1,logmargina2)
%  where: logmarginal1,logmarginal2 = vectors containing
%                                     the log-marginal posterior over a grid of rho values
%                          rho_vec = the vector of rho values used for log-marginals
% ---------------------------------------------------
% NOTES:
% log_marginals are returned by sar_marginal() function
% rho_vec is returned by lndet() function
% ---------------------------------------------------
%  RETURNS: a Bayes factor, a scalar
% ---------------------------------------------------

% written by:
% James P. LeSage, 7/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

 
nrho = length(rho_vec);

lmarginal = [lmarg1 lmarg2];

% now scale using all of the vectors of log-marginals
% we must scale before exponentiating 
adj = max(max(lmarginal));
madj = lmarginal - adj;

xx = exp(madj);

% trapezoid rule integration
yy = matmul(rho_vec,ones(nrho,2));
isum = zeros(1,2);
for j=1:2
    yj = yy(:,j);
    xj = xx(:,j);    
isum(1,j) = sum(diff(yj).*(xj(1:end-1)+xj(2:end))/2);
end;

% compute posterior probabilities
psum = sum(isum);
probs = isum/psum;
odds = (probs(1)+realmin)/(probs(2)+realmin);



function  out = sar_marginal(detval,epe0,eped,epe0d,nobs,nvar,a1,a2,nu1,d1,g,Qvec)
% PURPOSE: returns a vector of the log-marginal over a grid of rho-values
%          for the case of Zellner g-prior on beta
% -------------------------------------------------------------------------
% USAGE: out = sar_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,a1,a2,g,Qvec)
% where:       detval = an ngrid x 2 matrix with rho-values and lndet values
%                  e0 = y - x*b0;
%                 ed = Wy - x*bd;
%               epe0 = e0'*e0;
%               eped = ed'*ed;
%              epe0d = ed'*e0;
%               nobs = # of observations
%               nvar = # of explanatory variables
%                 a1 = parameter for beta prior on rho
%                 a2 = parameter for beta prior on rho
%                nu1 = prior parameter for sigma^2
%                 d1 = prior parameter for sigma^2
%                 g = g-prior value
%              Qvec = vectorized (g/(1+g)*(yhat'*yhat)
%                     over all rho-values
% -------------------------------------------------------------------------
% RETURNS: out = a structure variable
%        out = log marginal, a vector the length of detval
% -------------------------------------------------------------------------

% written by:
% James P. LeSage, 7/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

n = length(detval);
nm1o2 = (nobs+nu1-1)/2;
no2 = (nobs+nu1)/2;
% C is a constant of integration that can vary with nvars, so for model
% comparisions involving different nvars we need to include this
bprior = beta_prior(detval(:,1),a1,a2);
%C = log(bprior) + gammaln(nmk) - nmk*log(2*pi);
iota = ones(n,1);
z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
% add quadratic terms based on prior for beta
logdetx = (nvar/2)*log(g/(1+g));
gterm = 1/(1+g);
den = log(bprior) + logdetx*iota + detval(:,2) - nm1o2*log(nu1*d1 + gterm*z + Qvec);
kterm = gammaln(nm1o2) -no2*log(pi);
out = kterm + den;


function out = beta_prior(rvec,a1,a2)
% PURPOSE: construct beta-prior for rho over -1,1 interval
%-----------------------------------------------------------
% USAGE: out = beta_prior(a1,a2,rvec);
% where:    rvec = grid over rmin,rmax interval, an n x 1 vector
%           a1 = (optional) prior parameter (default = 1.1)
%           a2 = (optional) prior parameter (default = 1.1)
% RETURNS: out = nx1 vector of prior weights for rho
%          in the interval rmin,rmax
%-----------------------------------------------------------
% NOTES: increasing a1,a2 to 1.5,1.5 or 2.0,2.0 increases
%        prior weight placed on zero for rho, and decreases
%        weights on non-zero values for rho
% to see what the prior looks like:
% rvec = -1:0.01:1;
% a1 = 1.1; a2 = 1.1;
% bprior = beta_prior(rvec',a1,a2);
% plot(rvec,bprior);
%-----------------------------------------------------------

% written by:
% James P. LeSage, 4/2003
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if nargin == 1
a1 = 1.01;
a2 = 1.01;
elseif nargin == 2
    a2 = 1.01;
elseif nargin > 3
    error('beta_prior: wrong # of inputs');
end;

B = beta(a1,a2);
num = (1+rvec).^(a1-1);
num = num.*(1-rvec).^(a2-1);
den = 2^(a1+a2-1);
out = (1/B)*num/den;
out(1) = eps;
out(end) = eps;

function [vinew,vonew] = sample(vin,vout)
% PURPOSE: function used by sar BMA models to sample variables for changing model size
% ----------------------------------------------------------
% USAGE: [vinew vonew] = sample(vin,vout)
% where:   vin  = a 1 x nvar1 vector of variable #'s for
%                 variables included in the model
%          vout = a 1 x nvar2 vector of variable #'s for
%                 variables excluded from the model
% ----------------------------------------------------------
% RETURNS: vinew = a 1 x nvar1+1 or 1 x nvar1-1 vector of
%                  variable #'s in the new model
%          vonew = a 1 x nvar2+1 or 1 x nvar2-1 vector of
%                  variable #'s excluded from the new model
% ----------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com
% last modified June, 2004

% find size of variables in/out of the model
nv1 = length(vout); 
nv2 = length(vin);
nvar = nv1+nv2;

% decide on increase, move, decrease model size
coin = unif_rnd(1,0,1);
if coin < 0.33
    if nv1 == 0
        increase = 1;
    else
    increase = 0;
    end
elseif coin > 0.66
    if nv2 == 0
        increase = 0;
    else
    increase = 1;
    end
else
    increase = 2;
end;


switch increase
case {0} % decrease the # variables in the model
choose = round(unif_rnd(1,1,length(vout)));
vinew = [vin vout(choose)];
vonew = zeros(1,length(vout)-1);
cnt = 1;
for i=1:length(vout);
 if i~= choose
 vonew(cnt) = vout(i);
 cnt = cnt+1;
 end;
end; % end of for loop
case {1} % increase the # variables in the model
choose = round(unif_rnd(1,1,length(vin)));
vinew = zeros(1,length(vin)-1);
cnt = 1;
for i=1:length(vin);
 if i~= choose
 vinew(cnt) = vin(i);
 cnt = cnt+1;
 end;
end; % end of for loop
vonew = zeros(1,length(vout)+1);
cnt = 1;
for i=1:length(vout);
    vonew(cnt) = vout(cnt);
    cnt = cnt+1;
end;
vonew(cnt) = vin(choose);
case {2} % change a variable that is in with one that is out
    choose1 = round(unif_rnd(1,1,length(vout)));
    choose2 = round(unif_rnd(1,1,length(vin)));
vinew = vin;
vonew = vout;
for i=1:length(vout);
 if i == choose1
     for j=1:length(vin);
        if j == choose2
        vinew(j) = vout(i);
        vonew(i) = vin(j);
        end;
    end; % end of for j loop
 end; % end of if i == choose1
end; % end of for i loop


otherwise
disp('error in sample function');    
end; % end of switch   
    

