function results = recm_g(y,nlag,prior,ndraw,nomit,r)
% PURPOSE: Gibbs sampling estimates for Bayesian error correction 
%          model using Random-walk averaging prior
%          dy = A(L) DY  + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), rval/vi = ID chi(rval)/rval, rval = Gamma(m,k)
%          c = R A(L) + U, U = N(0,Z), Random-walk averaging prior          
%---------------------------------------------------
% USAGE: result = recm_g(y,nlag,prior,ndraw,nomit,r) 
% WHERE:    y    = an (nobs x neqs) matrix of y-vectors in levels
%           nlag = the lag length 
%          prior = a structure variable
%               prior.rval, rval prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on rval
%               prior.k,    informative Gamma(m,k) prior on rval 
%               prior.w,    an (neqs x neqs) matrix containing prior means
%                           (rows should sum to unity, see below)
%               prior.freq = 1 for annual, 4 for quarterly, 12 for monthly
%               prior.sig  = prior variance hyperparameter (see below)
%               prior.tau  = prior variance hyperparameter (see below)
%               prior.theta = prior variance hyperparameter (see below)             
%          ndraw = # of draws
%          nomit = # of initial draws omitted for burn-in       
%           r    = # of cointegrating relations to use
%                  (optional: this will be determined using
%                  Johansen's trace test at 95%-level if left blank)                                      
% priors for important variables:  N(w(i,j),sig) for 1st own lag
%                                  N(  0 ,tau*sig/k) for lag k=2,...,nlag
% priors for unimportant variables: N(w(i,j) ,theta*sig/k) for lag 1 
%                                   N(  0 ,theta*sig/k)    for lag k=2,...,nlag  
% e.g., if y1, y3, y4 are important variables in eq#1, y2 unimportant
%  w(1,1) = 1/3, w(1,3) = 1/3, w(1,4) = 1/3, w(1,2) = 0                                              
% typical values would be: sig = .1-.3, tau = 4-8, theta = .5-1  
% ---------------------------------------------------
% NOTES: - estimation is carried out in annualized growth terms 
% because the prior means rely on common (growth-rate) scaling of variables
% hence the need for a freq argument input.
%        - constant term included automatically  
%        - x-matrix of exogenous variables not allowed
%        - error correction variables are automatically
%          constructed using output from Johansen's ML-estimator 
% ---------------------------------------------------
% RETURNS a structure
% results.meth  = 'recm_g'
% results.nobs  = nobs, # of observations
% results.neqs  = neqs, # of equations
% results.nlag  = nlag, # of lags
% results.nvar  = nlag*neqs + r + 1, # of variables per equation
% results.freq  = freq
% results.coint = # of co-integrating relations (or r if input)
% results.weight= prior means weight matrix
% results.sig   = tightness hyperparameter
% results.tau   = tau hyperparameter
% results.theta = theta hyperparameter
% results.ndraw = # of draws
% results.nomit = # of draws omitted for burn-in
% results.r     = rval hyperparameter
% results.m     = m hyperparameter (if used)
% results.k     = k hyperparameter (if used)
% results.x     = cointegrating variables (nobs-freq,nx)
% results.nx    = # of cointegrating variables
% --- the following are referenced by equation # --- 
% results(eq).bdraw  = bhat draws (ndraws-nomit x nvar)
% results(eq).sdraw  = sige draws (ndraws-nomit x 1)
% results(eq).vmean  = mean of vi draws (nobs x 1)
% results(eq).rdraw  = r draws if m,k used (ndraw-nomit x 1)
% results(eq).y      = actual y-level values (nobs x 1)
% results(eq).dy     = actual y-growth rate values (nobs-nlag-freq,1)
% results(eq).time   = time in seconds taken for sampling
%---------------------------------------------------    
% SEE ALSO: becm_g, rvar_g, bvar_g, prt_varg 
%---------------------------------------------------
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

if nargin == 6 % user is specifying the # of error correction terms to
             % include -- get them using johansen()
 jres = johansen(y,0,nlag);
 % recover error correction vectors
 ecvectors = jres.evec;
        index = jres.ind;
 % construct r-error correction variables
 x = mlag(y(:,index),1)*ecvectors(:,1:r); 
   [nobs2 nx] = size(x);
   
elseif nargin == 5 % we need to find r
 jres = johansen(y,0,nlag);
 % find r = # significant co-integrating relations using
 % the trace statistic output
 trstat = jres.lr1;
 tsignf = jres.cvt;
 r = 0;
 for i=1:neqs;
  if trstat(i,1) > tsignf(i,2)
   r = i;
  end;
 end;
 % recover error correction vectors
 ecvectors = jres.evec;
        index = jres.ind;
 % construct r error correction variables
if r > 0
 x = mlag(y(:,index),1)*ecvectors(:,1:r); 
   [junk nx] = size(x);    
end;
else
 error('Wrong # of arguments to recm_g');
end;

% parse prior fieldnames
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

% pass on prior to rvar_g

% call RVAR using co-integrating variables as x-matrix
% call depends on whether we have an x-matrix or not
if nx ~= 0 
results = rvar_g(y,nlag,prior,ndraw,nomit,x);
else
results = rvar_g(y,nlag,prior,ndraw,nomit);
end;

results(1).meth = 'recm_g';
results(1).coint = r;
results(1).sig = sig;
results(1).weight = w;
results(1).tau = tau;
results(1).theta = theta;
results(1).index = index;
results(1).ndraw = ndraw;
results(1).nomit = nomit;




