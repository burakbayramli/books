function results = rvar_g(y,nlag,prior,ndraw,nomit,x);
% PURPOSE: Gibbs estimates for a Bayesian vector autoregressive 
%          model using the random-walk averaging prior 
%          y = A(L) Y + X B + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          c = R A(L) + U, U = N(0,Z), random-walk averaging prior
%          diffuse prior on B is used          
%---------------------------------------------------
% USAGE:  result = rvar_g(y,nlag,prior,ndraw,nomit,x)
% where:    y    = an (nobs x neqs) matrix of y-vectors (in levels)
%           nlag = the lag length
%          prior = a structure variable
%               prior.rval, r prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on r
%               prior.k,    informative Gamma(m,k) prior on r 
%               prior.w,    an (neqs x neqs) matrix containing prior means
%                           (rows should sum to unity, see below)
%               prior.freq = 1 for annual, 4 for quarterly, 12 for monthly
%               prior.sig  = prior variance hyperparameter (see below)
%               prior.tau  = prior variance hyperparameter (see below)
%               prior.theta = prior variance hyperparameter (see below)                
%          ndraw = # of draws
%          nomit = # of initial draws omitted for burn-in                 
%           x    = an (nobs x nx) matrix of deterministic variables
%                  (in any form, they are not altered during estimation)
%                  (constant term automatically included)                  
% priors for important variables:  N(w(i,j),sig) for 1st own lag
%                                  N(  0 ,tau*sig/k) for lag k=2,...,nlag               
% priors for unimportant variables: N(w(i,j) ,theta*sig/k) for lag 1 
%                                   N(  0 ,theta*sig/k)    for lag k=2,...,nlag  
% e.g., if y1, y3, y4 are important variables in eq#1, y2 unimportant
%  w(1,1) = 1/3, w(1,3) = 1/3, w(1,4) = 1/3, w(1,2) = 0                                              
% typical values would be: sig = .1-.3, tau = 4-8, theta = .5-1  
%---------------------------------------------------
% NOTES: - estimation is carried out in annualized growth terms 
% because the prior means rely on common (growth-rate) scaling of variables
%          hence the need for a freq argument input.
%        - constant term included automatically  
%---------------------------------------------------
% RETURNS: a structure
% results.meth   = 'rvar_g'
% results.nobs   = nobs, # of observations
% results.nadj   = nobs - nlag - freq
% results.neqs   = neqs, # of equations
% results.nlag   = nlag, # of lags
% results.nvar   = nlag*neqs+nx+1, # of variables per equation
% results.freq   = freq
% results.r      = rval hyperparameter 
% results.m      = m hyperparameter (if used)
% results.k      = k hyperparameter (if used)
% results.weight = prior means matrix
% results.sig    = prior hyperparameter
% results.tau    = prior hyperparameter
% results.theta  = prior hyperparameter
% results.nx     = # of deterministic variables
% results.x      = deterministic variables (nobs-freq,nx)
% results.ndraw  = # of draws
% results.nomit  = # of draws omitted for burn-in
% --- the following are referenced by equation # --- 
% results(eq).bdraw  = bhat draws (ndraws-nomit x nvar)
% results(eq).sdraw  = sige draws (ndraws-nomit x 1)
% results(eq).vmean  = mean of vi draws (nobs x 1)
% results(eq).rdraw  = r draws if m,k used (ndraw-nomit x 1)
% results(eq).y      = actual y-level values (nobs x 1)
% results(eq).dy     = actual y-growth rate values (nlag+freq+1:nobs,1)
% results(eq).time   = time in seconds taken for sampling
% ---------------------------------------------------    
% SEE ALSO: bvar_g, becm_g, recm_g, prt, prt_varg 
% ---------------------------------------------------
% References: LeSage and Krivelyova (1998) 
% ``A Spatial Prior for Bayesian Vector Autoregressive Models'',
% forthcoming Journal of Regional Science, (on http://www.econ.utoledo.edu)
% and
% LeSage and Krivelova (1997) (on http://www.econ.utoledo.edu)
% ``A Random Walk Averaging Prior for Bayesian Vector Autoregressive Models''

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs neqs] = size(y);


nx = 0;

if nargin == 6 % user is specifying deterministic variables
   [nobs2 nx] = size(x);
   
elseif nargin == 5 % no deterministic variables
nx = 0;
else
 error('Wrong # of arguments to rvar_g');
end;

% parse prior parameters
fields = fieldnames(prior);
nf = length(fields);
mm = 0; rval = 4; % rval = 4 is default
nu = 0; d0 = 0; % default to a diffuse prior on sige
for i=1:nf
    if strcmp(fields{i},'rval')
        rval = prior.rval; 
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval
    elseif strcmp(fields{i},'tau')
        tau = prior.tau;
    elseif strcmp(fields{i},'w')
        w = prior.w;       
       [wchk1 wchk2] = size(w);
       if (wchk1 ~= wchk2) 
       error('non-square w matrix in rvar_g');
       elseif wchk1 > 1
        if wchk1 ~= neqs
        error('wrong size w matrix in rvar_g');
        end;
       end;
    elseif strcmp(fields{i},'theta')
        theta = prior.theta;   
    elseif strcmp(fields{i},'sig')
        sig = prior.sig; 
    elseif strcmp(fields{i},'freq')
        freq = prior.freq;         
    end;
end;


results.meth = 'rvar_g';
results.sig = sig;
results.tau = tau;
results.theta = theta;
results.nobs = nobs;
results.nadj = nobs-nlag-freq;
results.neqs = neqs;
results.nlag = nlag;
results.weight = w;
results.ndraw = ndraw;
results.nomit = nomit;
results.freq = freq;
results.nx = nx;
if nx > 0
results.x = trimr(x,nlag+freq,0);
end;
if mm ~= 0
    results.m = mm;
    results.k = kk;
else
    results.r = rval;
end;


% transform y-levels to annualized growth rates
dy = growthr(y,freq);
dy = trimr(dy,freq,0);

% adjust nobs to account for seasonal differences and lags
nobse = nobs-freq-nlag;

% nvar 
 k = neqs*nlag+nx+1;
 nvar = k;
 
results.nvar = nvar;

y1 = mlag(dy,1);
y1 = trimr(y1,nlag,0);   % 1st own lags of the y-variables
xlag = nclag(dy,2,nlag); % lags 2 to nlag of the y-variables
xlag = trimr(xlag,nlag,0);
if nx > 0
x = trimr(x,nlag+freq,0); % truncate x variables for lags and diffs
end;
iota = ones(nobs,1);
iota = trimr(iota,nlag+freq,0);
dy = trimr(dy,nlag,0);    % truncate to feed lags

% form x-matrix of var plus deterministic variables
if nx ~= 0 
xmat = [xlag x iota];
else
xmat = [xlag iota];
end;

% form prior vector of means and matrix of variances
% for autoregressive parameters
% r = R beta + vmat

R = zeros(k,k);   
% only fill in 1's for lags, leave determininistic 
% and constant term elements set to zero
for i=1:neqs*nlag
 R(i,i) = 1.0;
end;

for j=1:neqs;    % ========> Equations loop

r = zeros(k,1);    % prior means 
vmat = eye(k)*100; % diffuse prior variance constant and deterministic

% set prior means for first lags  
% using weight matrix
for icnt = 1:neqs;
 r(icnt,1) = w(j,icnt);
end;
         
% use prior mean of zero  for lags 2 to nlag
% plus deterministic variables and constant
% already set by using r=zeros to start with

for ii=1:neqs;   % prior std deviations for 1st lags
 if w(j,ii) ~= 0
    vmat(ii,ii) = sig;
    else
    vmat(ii,ii) = theta*sig;
    end;
   end;
   
cnt = neqs+1;
for ii=1:neqs;   % prior std deviations for lags 2 to nlag
       if w(j,ii) ~= 0
 for kk=2:nlag;
    vmat(cnt,cnt) = tau*sig/kk;
    cnt = cnt + 1;
    end;
       else
 for kk=2:nlag;
    vmat(cnt,cnt) = theta*sig/kk;
    cnt = cnt + 1;
    end;       
       end;
end;

yvec = dy(:,j);
vmat = vmat.*vmat;  

% set up prior structure variable for theil_g
tprior.beta = r;
tprior.bcov = vmat;
tprior.rmat = R;
if mm ~= 0
tprior.m = mm;
tprior.k = kk;
else
tprior.rval = rval;
end;
% default diffuse prior on sige used
    
res = theil_g(yvec,[y1 xmat],tprior,ndraw,nomit);


% rearrange bhat parameters, t-statistics, tprobs in var order
bmat = zeros(ndraw-nomit,k);

% =====> rearrange bhat parameters in var order
cnt = 1;
 for i=1:nlag:k; % fills in lag 1 parameters
 bmat(:,i) = res.bdraw(:,cnt);
 cnt = cnt + 1;
 end;

cnt = 2;
lcnt = 2;
 for i=1:k-nx-1-neqs; % fills in lag 2 to nlag parameters
 bmat(:,cnt) = res.bdraw(:,neqs+i);
 cnt = cnt+1;
 lcnt = lcnt +1;
  if lcnt == nlag+1;
  cnt = cnt + 1;
  lcnt = 2;
  end;
 end;
for i=k-nx-1:k;
bmat(:,i) = res.bdraw(:,i);
end;

results(j).bdraw = bmat;
results(j).y = y(:,j);
results(j).dy = dy(:,j);
results(j).rdraw = res.rdraw;
results(j).sdraw = res.sdraw;
results(j).vmean = res.vmean;
results(j).time = res.time;
end;
% end of for j loop



