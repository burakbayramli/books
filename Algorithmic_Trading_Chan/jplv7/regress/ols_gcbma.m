function results = ols_gcbma(y,x,ndraw,prior)
% PURPOSE: MC^3 x-matrix specification for homoscedastic OLS model
% ------------------------------------------------------
% usage: results = ols_gcbma(y,x,ndraw,prior)
% where: % where: y = dependent variable vector (nobs x 1)
%        x = studentized or well-scaled x-matrix (you should include an
%        intercept)
%    ndraw = # of draws
%    prior = a structure variable with:
%            prior.g     = g-value for g-prior, default: max(1/n,1/k^2)
%            prior.nmodels = # of top models to save (default = 1000)
%            prior.nu    = informative Gamma(nu,d0) prior on sige
%            prior.d0    = default: nu=0,d0=0 (diffuse prior)
%-------------------------------------------------------------
% RETURNS:  a structure:
%          results.meth    = 'ols_gcbma'
%          results.munique = # of unique models found
%          results.nmodels = # of top models to save from input or (default = 1000)
%          results.freqs   = a matrix with frequency distribution of
%                            variables appearing in all unique models found
%          results.models  = a matrix with top nmodels information
%          results.allmodels = a matrix with info on all unique models found
%          results.mprob   = a vector of posterior model probabilities
%          results.vprob   = a vector with variable probabilities
%          results.nobs    = # of observations
%          results.nvar    = # of variables in x-matrix
%          results.ndraw   = # of draws
%          results.y       = y-vector from input (nobs x 1)
%          results.nu      = nu prior parameter
%          results.d0      = d0 prior parameter
%          results.time1   = time for BMA sampling
%          results.time2   = total time taken  
% --------------------------------------------------------------
% NOTES: 1) the model descriptions and log-marginals for all unique models found
%           are written  to a file lmarginal.ols
% --------------------------------------------------------------
% SEE ALSO: prt_bmao(), for printing results
%           model_averageo() function, that can be used to construct
%           estimates based on averaging over models using posterior
%           model probabilities as weights
% --------------------------------------------------------------
% REFERENCES: 
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

t1 = clock;

% error checking on inputs
[n junk] = size(y);
[n1 k] = size(x);
time1 = 0;
time2 = 0;

nobsa = n;
results.nobs  = n;
results.nvar  = k;
results.y = y;      

if n1 ~= n
error('ols_gcbma: x-matrix contains wrong # of observations');
end;

[nu,d0,sige,g,gmodels] = ols_parse(prior,x);

nmodels = gmodels;
% storage for draws
          nvar = k;
          mlog = [];
          msave = [];
          visits = zeros(ndraw,1);
          vsave = zeros(ndraw,nvar);

% ====== initializations
% compute this stuff once to save time
in = ones(n,1);

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
hwait = waitbar(0,'OLS BMA : MCMC sampling ...');
for i=1:ndraw; 
    
% choose new model
[vinew vonew] = sample(vin,vout);
for j=1:length(vinew);
vsave(i,vinew(1,j)) = 1.0;
end;

[j visits] = find_new(i,vsave,vinew,visits);

if i == 1 % first draw, compute logpost for initial model
lmarg = bmapost(y,x,vinew,nu,d0,g);
mlog =  [mlog
         lmarg];
msave = [msave
         vsave(i,:)];
cnt = cnt+1; % count of unique models found
  lmarg_old = lmarg;
end; % end of if i == 1


if visits(i,1) ~= 0 % we found a new model
cnt = cnt+1; % increase counter for new models
lmarg_new = bmapost(y,x,vinew,nu,d0,g);
mlog = [mlog
        lmarg_new];
msave  = [msave
         vsave(i,:)];

else % we have a model that we have already computed the log-marginal for
     % so, we could look it up, but it is faster to just compute it again
  lmarg_new = bmapost(y,x,vinew,nu,d0,g);
end;

bf = model_odds(lmarg_new,lmarg_old);
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

time1 = abs(etime(clock,t0));
results.time1 = time1;

results.munique = cnt;

% write out results for 1000 best models
out = [mlog msave];
clear mlog;
clear msave;
save lmarginal.ols out -ascii;

psum = 0.0;
posts = [];

load lmarginal.ols;

mlog = lmarginal(:,1);
models = lmarginal(:,2:end);
adj = max(mlog);
madj = mlog - adj;
  isum = exp(madj);
  postprob = isum/sum(isum);
  
[probs_sort,pind] = sort(postprob);
results.mprob = probs_sort;
msort = models(pind,:);
out = [msort probs_sort];

[no,junk] = size(out);

% return all unique models sampled
results.allmodels = out;
results.vprob = sum(out)/no;
results.freq = sum(out);

% just pull out nmodels requested by the user
if (nmodels > no) % the user wants more models output than were found
    nmodels = no; % we can only output what was found
else
    % the user is okay
end;
outn = out(no-nmodels+1:no,:);
results.models = msort(no-nmodels+1:no,:);

results.ndraw = ndraw;
results.nu = nu;
results.d0 = d0;

time2 = abs(etime(clock,t1));
results.time2 = time2;


function [nu,d0,sige,g,gmodels] = ols_parse(prior,x)
% PURPOSE: parses input arguments for ols_gcbma models
% ---------------------------------------------------
%  USAGE: [nu,d0,rho,sige,g,gmodels] = 
%                           ols_parse(prior,x)
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

sige = 1.0;
nu = 0;
d0 = 0;

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
    end;
 end;
else, % the user has input a blank info structure
      % so we use the defaults
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

function lmout = bmapost(y,xall,vin,nu1,d1,g)
% PURPOSE: evaluates log marginal posterior of OLS bma model
%          (OLS Bayesian model averaging)
%-----------------------------------------------------------
% USAGE: lpost = bmapost(y,xall,vin,nu1,d1,g)
% where:   y = dependent variable vector (nobs x 1)
%       xall = explanatory variables matrix (nobs x k)
%        vin = a 1xk vector of columns to use from x, e.g. [1 3 5]
%        nu1 = prior parameter on sigma
%        d1  = prior parameter on sigma
%          g = g prior hyperparameter
%-----------------------------------------------------------
% RETURNS: lpost = log marginal posterior, a scalar
%                  containing the log marginal 
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

iota = ones(nobs,1);
xt = [iota xall(:,vin)];
[nobs,k] = size(xt);
nm1o2 = (nobs-1)/2;
no2 = nobs/2;

% now compute vectorized log-marginal likelihood
% to use in trapezoid numerical integration over rho
          xpx = xt'*xt;
          b0 = (xpx)\(xt'*y);
          e0 = y - xt*b0;
          epe0 = (1/(1+g))*(e0'*e0);
          ybar = mean(y);
          yhat = y - ybar*iota;
          Qterm = (g/(1+g))*(yhat'*yhat);
          logdetx = (nvar/2)*log(g/(1+g));
          den = logdetx - nm1o2*log(nu1*d1 + epe0 + Qterm);
          kterm = gammaln(nm1o2) -no2*log(pi);
          
lmout = kterm + den;


function odds = model_odds(lmarg1,lmarg2)
% PURPOSE: computes Bayes factor for 2 models using log-marginals as input
% ---------------------------------------------------
%  USAGE: probs = model_odds(logmarginal1,logmargina2)
%  where: logmarginal1,logmarginal2 = the log-marginal posterior for the
%  two models
% ---------------------------------------------------
%  RETURNS: a Bayes factor, a scalar
% ---------------------------------------------------

% written by:
% James P. LeSage, 6/2004
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


lmarginal = [lmarg1 lmarg2];

% now scale log-marginals before exponentiating 
adj = max(lmarginal);
madj = lmarginal - adj;

xx = exp(madj);

% compute posterior probabilities
psum = sum(xx);
probs = xx/psum;
odds = (probs(1)+realmin)/(probs(2)+realmin);


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
    

