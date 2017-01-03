function results =  probit_g(y,x,ndraw,nomit,prior,seed)
% PURPOSE: MCMC sampler for the Bayesian heteroscedastic Probit model  
%          y = X B + E, E = N(0,V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          B = N(c,T)
% --------------------------------------------------------------
% USAGE: results =  probit_g(y,x,ndraw,nomit,prior,start)
% where: y = nobs x 1 independent variable vector
%        x = nobs x nvar explanatory variables matrix
%       ndraw = # of draws
%       nomit = # of initial draws omitted for burn-in
%       prior = a structure for prior information input
%               prior.beta, prior means for beta,  c above (default=0)
%               priov.bcov, prior beta covariance, T above (default=1e+12)
%               prior.rval, r prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on r
%               prior.k,    informative Gamma(m,k) prior on r
%       seed = (optional) string for the random number generator seed
%              e.g., seed = num2str(1234);
%---------------------------------------------------------------
% RETURNS: a structure:
%          results.meth  = 'probit_g'
%          results.bdraw = bhat draws (ndraw-nomit x nvar)
%          results.vmean = mean of vi draws (nobs x 1)
%          results.ymean = mean of y draws (nobs x 1)
%          results.sdraw = sige draws (ndraw-nomit x 1)
%          results.rdraw = r-value draws (ndraw-nomit x 1), if Gamma(m,k) prior used
%          results.pmean = b prior means
%          results.pstd  = b prior std deviations
%          results.m     = prior m-value for r hyperparameter (if input)
%          results.k     = prior k-value for r hyperparameter (if input)
%          results.r     = value of hyperparameter r (if input)
%          results.r2mf  = McFadden R-squared
%          results.rsqr  = Estrella R-squared
%          results.nobs  = # of observations
%          results.nvar  = # of variables
%          results.ndraw = # of draws
%          results.nomit = # of initial draws omitted
%          results.y     = actual observations
%          results.yhat  = mean of posterior probs predicted for y
%          results.x     = x-matrix
%          results.time  = time taken for sampling
%          results.seed  = random number seed (from input)
%----------------------------------------------------------------
% NOTE: use either improper prior.rval 
%       or informative Gamma prior.m, prior.k, not both of them
%---------------------------------------------------------------
% References: James H. Albert and Siddhartha Chib
%             Bayesian Analysis of Binary and Polychotomous
%             Response Data JASA, June 1993, pp. 669            
%----------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[n k] = size(x);   

% error checking on input

% check for all 1's or all 0's
tmp = find(y ==1);
chk = length(tmp); 
[nobs junk] = size(y);
if chk == nobs
   error('probit_g: y-vector contains all ones');
elseif chk == 0
   error('probit_g: y-vector contains no ones');
end;

b0 = ones(k,1);
sflag = 0;

if nargin == 6   % user-supplied a seed       
    if ~isstruct(prior)
    error('probit_g: must supply the prior as a structure variable');
    end;       
       
sflag = 1;
in = ones(n,1);
    
fields = fieldnames(prior);
nf = length(fields);
mm = 0; 
rval = 4;  % rval = 4 is default
nu = 0;    % default diffuse prior for sige
d0 = 0;
c = zeros(k,1);
T = eye(k)*1e+12;
for i=1:nf
    if strcmp(fields{i},'rval')
        rval = prior.rval; 
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval
    elseif strcmp(fields{i},'beta')
        c = prior.beta;
    elseif strcmp(fields{i},'bcov')
        T = prior.bcov;
    elseif strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;
    end;
end;


elseif  nargin == 5  % probit maximum likelihood starting values
b0 = ones(k,1); 
V = ones(n,1); in = ones(n,1); % initial value for V 

fields = fieldnames(prior);
nf = length(fields);
mm = 0; 
rval = 4;  % rval = 4 is default
nu = 0;    % default diffuse prior for sige
d0 = 0;
c = zeros(k,1);
T = eye(k)*1e+12;
for i=1:nf
    if strcmp(fields{i},'rval')
        rval = prior.rval; 
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval
    elseif strcmp(fields{i},'beta')
        c = prior.beta;
    elseif strcmp(fields{i},'bcov')
        T = prior.bcov;
    elseif strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;
    end;
   end;
   
elseif nargin == 4 % use default prior
mm = 0; 
rval = 4;  % rval = 4 is default
nu = 0;    % default diffuse prior for sige
d0 = 0;
c = zeros(k,1);
T = eye(k)*1e+12;   
V = ones(n,1); in = ones(n,1); % initial value for V 

else
error('Wrong # of arguments to probit_g');
end;


[checkk,junk] = size(c);
if checkk ~= k
error('probit_g: prior means are wrong');
elseif junk ~= 1
error('probit_g: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('probit_g: prior bcov is wrong');
elseif junk ~= k
error('probit_g: prior bcov is wrong');
end;

Q = inv(T); 
Qpc = Q*c;
          
bsave = zeros(ndraw-nomit,k);    % allocate storage for results
ymean = zeros(n,1); 
rsave = zeros(ndraw-nomit,1);
vmean = zeros(n,1);
yhat = zeros(n,1);

sv = 1.0;          
yin = y;   % save original y-values
bhat = b0; % starting value for beta
        
hwait = waitbar(0,'Gibbs sampling ...');            
t0 = clock;          

for i=1:ndraw; %  Start the sampling

          xstar = matmul(x,sqrt(V));
          ystar = y.*sqrt(V);

          % update z 
          lp=xstar*bhat; 
	      ind = find(yin == 0);
	      y(ind,1) = normrt_rnd(lp(ind,1),(V(ind,1)),0);
	      ind = find(yin == 1);
	      y(ind,1) = normlt_rnd(lp(ind,1),(V(ind,1)),0);
  

          % update beta
          xpxi = inv(xstar'*xstar + Q); 
          xpy = xstar'*ystar + Qpc;
          bhat = xpxi*xpy;
          bhat = norm_rnd(xpxi) + bhat;
        
          % update V
          e = y - x*bhat;  
          chiv = chis_rnd(n,rval+1);
          vi = ((e.*e) + in*rval)./chiv;
          V = in./vi; 
          if mm ~= 0
           rval = gamm_rnd(1,mm,kk);  % update rval
          end;             


if i > nomit % if we are past burn-in, save the draws
    bsave(i-nomit,:) = bhat';
    ymean = ymean + lp;
    vmean = vmean + vi;
    yhat = yhat + stdn_cdf(y);

    if mm~= 0
        rsave(i-nomit,1) = rval;
    end;          
end; % end of if i > nomit
waitbar(i/ndraw);
end;     % End the sampling
gtime = etime(clock,t0);
close(hwait);

vmean = vmean/(ndraw-nomit);
ymean = ymean/(ndraw-nomit);
yhat = yhat/(ndraw-nomit);

bmean = mean(bsave);
% compute McFadden R-squared
tmp = find(yin ==1); % find ones
P = length(tmp); 
cnt0 = n-P;
cnt1 = P;
P = P/n; % proportion of 1's
like0 = n*(P*log(P) + (1-P)*log(1-P));    % restricted likelihood
like1 = pr_like(bmean',yin,x);            % unrestricted Likelihood
r2mf = 1-(abs(like1)/abs(like0));         % McFadden pseudo-R2 
% compute Estrella R-squared
term0 = (2/n)*like0;
term1 = 1/(abs(like1)/abs(like0))^term0;
rsqr = 1-term1;                           % Estrella R2

% return results
results.meth  = 'probit_g';
results.r2mf = r2mf;
results.rsqr = rsqr;
results.bdraw = bsave;
results.pmean = c;
results.pstd  = sqrt(diag(T));
results.vmean = vmean;
results.ymean = ymean;
results.yhat = yhat;
if mm~= 0
results.rdraw = rsave;
results.m     = mm;
results.k     = kk;
else
results.r     = rval;
results.rdraw = rsave;
end;
results.nobs  = n;
results.nvar  = k;
results.y     = yin;
results.x     = x;
results.time = gtime;
results.ndraw = ndraw;
results.nomit = nomit;
results.pflag = 'plevel';

