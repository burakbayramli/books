function results = becm_g(y,nlag,prior,ndraw,nomit,r)
% PURPOSE: Gibbs sampling estimates for Bayesian error correction 
%          model using Minnesota-type prior
%          dy = A(L) DY  + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), rval/vi = ID chi(rval)/rval, rval = Gamma(m,k)
%          c = R A(L) + U, U = N(0,Z), Minnesota prior
%---------------------------------------------------
% USAGE: result = becm(y,nlag,prior,ndraw,nomit,r) 
% where:    y    = an (nobs x neqs) matrix of y-vectors in levels
%           nlag = the lag length
%          prior = a structure variable
%               prior.weight, Litterman's weight (matrix or scalar)
%               prior.decay,  Litterman's lag decay = lag^(-decay) 
%               prior.rval, rval prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on r
%               prior.k,    informative Gamma(m,k) prior on r  
%          ndraw = # of draws
%          nomit = # of initial draws omitted for burn-in       
%           r    = # of cointegrating relations to use
%                  (optional: this will be determined using
%                  Johansen's trace test at 95%-level if left blank)                                    
% NOTES: - constant vector automatically included
%        - error correction variables are automatically
%          constructed using output from Johansen's ML-estimator 
%---------------------------------------------------
% RETURNS a structure
% results.meth  = 'becm_g'
% results.nobs  = nobs, # of observations
% results.neqs  = neqs, # of equations
% results.nlag  = nlag, # of lags
% results.nvar  = nlag*neqs+nx+1, # of variables per equation
% results.coint = # of co-integrating relations (or r if input)
% results.tight = tightness hyperparameter
% results.weight= weight matrix or scalar
% results.decay = lag decay hyperparameter
% results.m  = prior m-value for r hyperparameter (if input)
% results.k  = prior k-value for r hyperparameter (if input)
% results.r  = value of hyperparameter r (if input)
% results.ndraw  = # of draws
% results.nomit  = # of initial draws omitted
% results.x      = cointegrating variables matrix (nobs x nx)
% results.nx     = # of cointegrating relations
% --- the following are referenced by equation # --- 
% results(eq).bdraw = bhat draws for equation eq
% results(eq).vmean = mean of vi draws for equation eq 
% results(eq).sdraw = sige draws for equation eq
% results(eq).rdraw = r-value draws for eq, if Gamma(m,k) prior 
% results(eq).y     = actual observations for eq (nobs x 1)
% results(eq).dy    = actual y in 1st difference form (nobs-1 x 1)
% results(eq).time  = time taken for sampling eq
% ---------------------------------------------------    
%  SEE ALSO: bvar_g, rvar_g, recm_g, prt_varg 
% ---------------------------------------------------
%  References: James P. LeSage, 
% ``A Comparison of the Forecasting Ability of ECM and VAR Models'',
%  Review of Economics and Statistics,  1990, Vol 72, number 4, pp. 664-671.

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs neqs] = size(y);

nx = 0;

% error checking on input
if ~isstruct(prior)
    error('becm_g: must supply the prior as a structure variable');
end;

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
 x = mlag(y(:,index),1)*ecvectors(:,1:r); 
   [junk nx] = size(x);    
else
 error('Wrong # of arguments to becm_g');
end;

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
    elseif strcmp(fields{i},'tight')
        tight = prior.tight;
        if tight < 0.01
        warning('Tightness less than 0.01 in becm_g');
        elseif tight > 1.0
        warning('Tightness greater than unity in becm_g');
        end;
    elseif strcmp(fields{i},'weight')
        weight = prior.weight;       
       [wchk1 wchk2] = size(weight);
       if (wchk1 ~= wchk2) 
       error('non-square weight matrix in becm_g');
       elseif wchk1 > 1
        if wchk1 ~= neqs
        error('wrong size weight matrix in becm_g');
        end;
       end;
    elseif strcmp(fields{i},'decay')
        decay = prior.decay;    
        if decay < 0
        error('Negative lag decay in becm_g');
        end;       
    end;
end;


if nlag < 1
error('Lag length less than 1 in becm_g');
end;

[nobs nvar] = size(x);
if nlag > nobs
error('Lag length exceeds observations in becm_g');
end;

% nvar adjusted for constant term 
 k = neqs*nlag+nx+1;
 nvar = k;

% transform to 1st difference form
dy = tdiff(y,1);
dy = trimr(dy,1,0); % account for differencing
x = trimr(x,1,0);   % account for differencing

% pass prior structure variable in call to bvar_g

% call BVAR using 1st difference and co-integrating variables
% call depends on whether we have an x-matrix or not
if nx ~= 0 
results = bvar_g(dy,nlag,ndraw,nomit,prior,x);
else
results = bvar_g(dy,nlag,ndraw,nomit,prior);
end;

nobst = length(x);
results(1).meth = 'becm_g';
results(1).coint = nx;
results(1).index = index;
if nx > 0
results(1).x = x;
end;
% delete results.nx fieldname returned by bvar_g
for j=1:neqs;
results(j).y = y(:,j);
results(j).dy = dy(:,j);

end;




