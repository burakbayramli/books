function result = bvar_g(x,nlag,ndraw,nomit,prior,xx);
% PURPOSE: Gibbs sampling estimates for Bayesian vector 
%          autoregressive model using Minnesota-type prior
%          y = A(L) Y + X B + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          c = R A(L) + U, U = N(0,Z), Minnesota prior
%          a diffuse prior is used for B associated with deterministic
%          variables
%---------------------------------------------------
% USAGE:  result = bvar_g(y,nlag,ndraw,nomit,prior,x)
% WHERE:    y    = an (nobs x neqs) matrix of y-vectors
%           nlag = the lag length
%          ndraw = # of draws
%          nomit = # of initial draws omitted for burn-in   
%          prior = a structure variable
%               prior.tight,  Litterman's tightness hyperparameter
%               prior.weight, Litterman's weight (matrix or scalar)
%               prior.decay,  Litterman's lag decay = lag^(-decay) 
%               prior.rval, r prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on r
%               prior.k,    informative Gamma(m,k) prior on r      
%          x     = an optional (nobs x nx) matrix of variables
% NOTE:  constant vector automatically included
%---------------------------------------------------
% RETURNS: a structure:
% results.meth   = 'bvar_g'
% results.nobs   = nobs, # of observations
% results.neqs   = neqs, # of equations
% results.nlag   = nlag, # of lags
% results.nvar   = nlag*neqs+1+nx, # of variables per equation
% results.tight  = overall tightness hyperparameter
% results.weight = weight scalar or matrix hyperparameter
% results.decay  = lag decay hyperparameter
% results.m  = prior m-value for r hyperparameter (if input)
% results.k  = prior k-value for r hyperparameter (if input)
% results.r  = value of hyperparameter r (if input)
% results.ndraw  = # of draws
% results.nomit  = # of initial draws omitted
% results.nx     = # of deterministic variables
% results.x      = deterministic variables matrix (nobs x nx)
% --- the following are referenced by equation # --- 
% results(eq).bdraw = bhat draws for equation eq
% results(eq).vmean = mean of vi draws for equation eq 
% results(eq).sdraw = sige draws for equation eq
% results(eq).rdraw = r-value draws for eq, if Gamma(m,k) prior 
% results(eq).y     = actual observations for eq (nobs x 1)
% results(eq).time   = time taken for sampling eq
% ---------------------------------------------------
% SEE ALSO:  bvar, var, ecm, rvar, plt, prt
% ---------------------------------------------------
% REFERENCES:  LeSage and Krivelova (1997) (on http://www.econ.utoledo.edu)
% ``A Random Walk Averaging Prior for Bayesian Vector Autoregressive Models''
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs neqs] = size(x);

% error checking on input
if ~isstruct(prior)
    error('bvar_g: must supply the prior as a structure variable');
    
elseif  nargin == 6  % deterministic variables
[nobs2 nx] = size(xx);
   if (nobs2 ~= nobs)
   error('X and Y-matrices in bvar_g have different # of obs');
   end;
result.x = xx;

elseif nargin == 5 % no deterministic variables
nx = 0;

else 
error('Wrong # of arguments to bvar_g');
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
        warning('Tightness less than 0.01 in bvar_g');
        elseif tight > 1.0
        warning('Tightness greater than unity in bvar_g');
        end;
    elseif strcmp(fields{i},'weight')
        weight = prior.weight;       
       [wchk1 wchk2] = size(weight);
       if (wchk1 ~= wchk2) 
       error('non-square weight matrix in bvar_g');
       elseif wchk1 > 1
        if wchk1 ~= neqs
        error('wrong size weight matrix in bvar_g');
        end;
       end;
    elseif strcmp(fields{i},'decay')
        decay = prior.decay;    
        if decay < 0
        error('Negative lag decay in bvar_g');
        end;       
    end;
end;


if nlag < 1
error('Lag length less than 1 in bvar_g');
end;

[nobs nvar] = size(x);
if nlag > nobs
error('Lag length exceeds observations in bvar_g');
end;

% adjust nobs to feed the lags
nobse = nobs - nlag;

% nvar adjusted for constant term
k = neqs*nlag + 1 + nx;
nvar = k;

% fill-in easy stuff
result.meth = 'bvar_g';
result.nlag = nlag;
result.nvar = nvar;
result.nobs = nobse;
result.neqs = neqs;
result.tight = tight;
result.decay = decay;
result.weight = weight;
result.ndraw = ndraw;
result.nomit = nomit;
result.r = rval;
result.nx = nx;
if nx > 0
result.x = xx;
end;

% generate lagged rhs matrix
xlag = mlag(x,nlag);

% do scaling here using fuller y-vector information
% determine scale factors using univariate AR model

scale = zeros(neqs,1);
scale2 = zeros(neqs,neqs);

for j=1:neqs
   ytmp = x(1:nobs,j);
   scale(j,1) = scstd(ytmp,nobs,nlag);
end;

for j=1:neqs;
   for i=1:neqs;
   scale2(i,j) = scale(j,1)/scale(i,1);
   end;
end;

% form x-matrix 
if nx
xmat = [xlag(nlag+1:nobs,:) xx(nlag+1:nobs,:) ones(nobs-nlag,1)];
else
xmat = [xlag(nlag+1:nobs,:) ones(nobs-nlag,1)];
end;

% Form prior to feed down to ols_g
[nw1 nw2] = size(weight);
if nw1 == 1  % case of a scalar symmetric weight matrix
wght = ones(neqs,neqs)*weight;
 for i=1:neqs;
 wght(i,i) = 1.0;
 end;
else % general prior weight matrix
wght = weight;
end;


% pull out each y-vector and run ols_g regressions
for eqn=1:neqs;

yvec = x(nlag+1:nobs,eqn);

% find Doan's sigma(i,j,l)
sigma = zeros(nvar,1);

k = 1;
for j=1:neqs;
 for l=0:nlag-1;
  ldecay = (l+1)^decay;
  ldecay = 1.0/ldecay;
  sigma(k,1) = (tight*wght(eqn,j)*ldecay)*scale2(j,eqn);
  k = k+1;
 end;
end;

% setup prior R-matrix
% R = diagonal matrix with scale(i,1)/S(i,j,l)
R = zeros(nvar,nvar);

% N.B. we don't want to divide by zero 
% (diffuse prior on the x-variables and constant term) 
% so we use nvar-nx-1  

for i=1:nvar-nx-1;
R(i,i) = scale(eqn,1)/sigma(i,1);
end;

% setup prior c-vector
% equal to scale(i,1)/S(i,j,l) x prior mean

c = zeros(nvar,1);
cind = (eqn-1)*nlag+1;
if eqn == 1
cind = 1;
end;
c(cind,1) = scale(eqn,1)/sigma(cind,1);

oprior.beta = c;
oprior.rmat = R;
oprior.bcov = eye(nvar);
if mm ~= 0;
oprior.m = mm;
oprior.k = kk;
else
oprior.rval = rval;
end;

bresult = theil_g(yvec,xmat,oprior,ndraw,nomit);

result(eqn).bdraw = bresult.bdraw;     
result(eqn).vmean = bresult.vmean;     
result(eqn).rdraw = bresult.rdraw;     
result(eqn).sdraw = bresult.sdraw;    
result(eqn).y = x(:,eqn); 
result(eqn).time = bresult.time;
end;
