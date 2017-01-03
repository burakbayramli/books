function results = theil_g(y,x,prior,ndraw,nomit)
% PURPOSE: Gibbs estimates for the BVAR heteroscedastic linear model
%          intended to be called only by bvar_g()
%          y = X B + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          c = R B + U, U = N(0,Z), sige = gamma(nu,d0)    
%---------------------------------------------------
% USAGE: results = theil_g(y,x,prior,ndraw,nomit,start)
% where: y    = dependent variable vector
%        x    = independent variables matrix of rank(k)
%       prior = a structure based on: c = R B + U
%               prior.beta, prior means for beta,   c above
%               priov.bcov, prior beta covariance , Z above
%               prior.rmat, R-matrix,      R above 
%               prior.rval, r prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on r
%               prior.k,    informative Gamma(m,k) prior on r
%               prior.nu,   informative Gamma(nu,d0) prior on sige
%               prior.d0    informative Gamma(nu,d0) prior on sige
%                           default for above: diffuse prior
%       ndraw = # of draws
%       nomit = # of initial draws omitted for burn-in
% ---------------------------------------------------
% RETURNS: a structure:
%          results.meth  = 'theil_g'
%          results.bdraw = bhat draws (nvar x ndraw-nomit)
%          results.vmean = mean of vi draws (nobs x 1)
%          results.sdraw = sige draws (ndraw-nomit x 1)
%          results.rdraw = r-value draws (ndraw-nomit x 1), if Gamma(m,k) prior 
%          results.pmean = b prior means
%          results.pstd  = b prior std deviation
%          results.m     = prior m-value for r hyperparameter (if input)
%          results.k     = prior k-value for r hyperparameter (if input)
%          results.r     = value of hyperparameter r (if input)
%          results.nu    = prior nu-value for sige prior
%          results.d0    = prior d0-value for sige prior
%          results.nobs  = # of observations
%          results.nvar  = # of variables
%          results.ndraw = # of draws
%          results.nomit = # of initial draws omitted
%          results.y     = actual observations
%          results.x     = x-matrix
%          results.time  = time taken for sampling
% --------------------------------------------------
% NOTE: use either improper prior.rval 
%       or informative Gamma prior.m, prior.k, not both of them
%---------------------------------------------------
% SEE ALSO: bvar_g(), ols_g()
%---------------------------------------------------
% REFERENCES: Geweke (1993)  'Bayesian Treatment of the 
% Independent Student-$t$ Linear Model', Journal of Applied
% Econometrics, 8, s19-s40.
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[n k] = size(x);   

% error checking on input
if ~isstruct(prior)
    error('theil_g: must supply the prior as a structure variable');
    
elseif  nargin == 5  % ols starting values
b0 = (x'*x)\(x'*y);  % Find ols values as initial starting values
sige = (y-x*b0)'*(y-x*b0)/(n-k);
V = ones(n,1); in = ones(n,1); % initial value for V    
else
error('Wrong # of arguments to theil_g');
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
    elseif strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;           
    end;
end;

r = prior.beta;
R = prior.rmat;
T = prior.bcov;

vi = inv(T); 
vterm = R'*vi*R;
vmean = R'*vi*r;  
 
bsave = zeros(ndraw-nomit,k);    % allocate storage for results
ssave = zeros(ndraw-nomit,1); 
rsave = zeros(ndraw-nomit,1);
vsave = zeros(n,1);

t0 = clock;
for i=1:ndraw; % Start the sampling
          ystar = y.*sqrt(V); 
          xstar = matmul(x,sqrt(V));
          xpxi = inv(xstar'*xstar + vterm); 
          xpy = (xstar'*ystar + vmean); 
          % update b  
          b = xpxi*xpy;                 
          b = norm_rnd(xpxi) + b; % draw MV normal with mean(b), var(b)

         % update sige 
          nu1 = n + nu; 
          e = ystar - xstar*b;
          d1 = d0 + e'*e;
          chi = chis_rnd(1,nu1);
          t2 = chi/d1;
          sige = 1/t2;
   
         % update vi
         e = y - x*b;
         chiv = chis_rnd(n,rval+1);   
         vi = ((e.*e./sige) + in*rval)./chiv;
         V = in./vi;   
  
         % update rval
         if mm ~= 0           
         rval = gamm_rnd(1,1,mm,kk);  
         end;
    if i > nomit % if we are past burn-in, save the draws
    bsave(i-nomit,:) = b';
    ssave(i-nomit,1) = sige;
    vsave = vsave + vi;
    if mm~= 0
        rsave(i-nomit,1) = rval;
    end;
end;

end;          % End the sampling
gtime = etime(clock,t0);

% return results
results.meth  = 'theil_g';
results.bdraw = bsave;
results.pmean = r;
results.pstd  = diag(T);
results.vmean = vsave/(ndraw-nomit);
results.sdraw = ssave;
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
results.y     = y;
results.x     = x;
results.nu    = nu;
results.d0    = d0;
results.time = gtime;
results.ndraw = ndraw;
results.nomit = nomit;

