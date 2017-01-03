function results = gwr_g(y,x,ndraw,nomit,prior)
% PURPOSE: Gibbs estimates for the Bayesian heteroscedastic GWR model
%          called only by bgwrv(), bgwr() 
%          y = X B + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          B = N(c,T),  sige = gamma(nu,d0)    
%---------------------------------------------------
% USAGE: results = gwr_g(y,x,ndraw,nomit,prior)
% where: y    = dependent variable vector
%        x    = independent variables matrix of rank(k)
%       ndraw = # of draws
%       nomit = # of initial draws omitted for burn-in
%       prior = a structure for prior information input
%               prior.beta, prior means for beta,   c above
%               priov.bcov, prior beta covariance , T above
%               prior.rval, r prior hyperparameter, default=4
%               prior.m,    informative Gamma(m,k) prior on r
%               prior.k,    informative Gamma(m,k) prior on r
%               prior.nu,   informative Gamma(nu,d0) prior on sige
%               prior.d0    informative Gamma(nu,d0) prior on sige
%                           default for above: nu=0,d0=0 (diffuse prior)
% ---------------------------------------------------
% RETURNS: a structure:
%          results.meth  = 'gwr_g'
%          results.bdraw = bhat draws (ndraw-nomit x nvar)
%          results.vmean = mean of vi draws (nobs x 1)
%          results.sdraw = sige draws (ndraw-nomit x 1)
% --------------------------------------------------
% NOTES: This is a truncated version of ols_g that
%        was customized for speed for bgwr, bgwrv
%---------------------------------------------------
% SEE ALSO: bgwr, bgwrv
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


if nargin ~= 5
error('Wrong # of arguments to gwr_g');
end;

b0 = (x'*x)\(x'*y);  % Find ols values as initial starting values
sige = (y-x*b0)'*(y-x*b0)/(n-k); 
V = ones(n,1); in = ones(n,1); % initial value for V  

fields = fieldnames(prior);
nf = length(fields);
rval = 4; % rval = 4 is default
nu = 0; d0 = 0; % default to a diffuse prior on sige
c = zeros(k,1); T = eye(k)*1e+12;
mm = 0;
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
    elseif strcmp(fields{i},'beta');
    c = prior.beta;
    elseif strcmp(fields{i},'bcov');
    T = prior.bcov;
    end;
 end;

[checkk,junk] = size(c);
if checkk ~= k
error('gwr_g: prior means are wrong');
elseif junk ~= 1
error('gwr_g: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('gwr_g: prior bcov is wrong');
elseif junk ~= k
error('gwr_g: prior bcov is wrong');
end;


Q = inv(T);
Qpc = Q*c;

bsave = zeros(ndraw-nomit,k);    % allocate storage for results
ssave = zeros(ndraw-nomit,1); 
rsave = zeros(ndraw-nomit,1);
vmean = zeros(n,1);

for i=1:ndraw; % Start the sampling 
          ystar = y.*sqrt(V); 
          xstar = matmul(x,sqrt(V));
          xpxi = inv(xstar'*xstar + sige*Q); 
          xpy = (xstar'*ystar + sige*Qpc); 
          % update b  
          b = xpxi*xpy;    
       a = chol(xpxi);
       b = sqrt(sige)*a'*randn(k,1) + b;          

         % update sige 
          nu1 = n + nu; 
          e = ystar - xstar*b;
          d1 = d0 + e'*e;
          chi = chis_rnd(1,nu1);
    sige = d1/chi;
         
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
    vmean = vmean + vi;
    if mm~= 0
        rsave(i-nomit,1) = rval;
    end;
end;

end;          % End the sampling

vmean = vmean/(ndraw-nomit);

% return results
results.meth  = 'gwr_g';
results.bdraw = bsave;
results.vmean = vmean;
results.sdraw = ssave;
if mm~= 0
results.rdraw = rsave;
results.m     = mm;
results.k     = kk;
else
results.rdraw = rsave;
end;

