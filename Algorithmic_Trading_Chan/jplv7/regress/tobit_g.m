function results =  tobit_g(y,x,ndraw,nomit,prior,start)
% PURPOSE: MCMC sampler for Bayesian Tobit model  
%          y = X B + E, E = N(0,sige*V), 
%          V = diag(v1,v2,...vn), r/vi = ID chi(r)/r, r = Gamma(m,k)
%          B = N(c,T),  sige = gamma(nu,d0)    
%----------------------------------------------------------------
% USAGE: result =  tobit_g(y,x,ndraw,nomit,prior,start)
% where: y = nobs x 1 independent variable vector
%        x = nobs x nvar explanatory variables matrix
%    ndraw = # of draws
%    nomit = # of initial draws omitted for burn-in
%    prior = a structure variable for prior information input
%            prior.beta, prior means for beta,  c above (default=0)
%            priov.bcov, prior beta covariance, T above (default=1e+12)
%            prior.rval, r prior hyperparameter, default=4
%            prior.m,    informative Gamma(m,k) prior on r
%            prior.k,    informative Gamma(m,k) prior on r 
%                        default for above: not used, rval=4 is used
%            prior.nu,   informative Gamma(nu,d0) prior on sige
%            prior.d0    informative Gamma(nu,d0) prior on sige
%                        default for above: nu=0,d0=0 (diffuse prior)
%            prior.trunc = 'left' or 'right' (default = 'left')
%            prior.limit = value for censoring (default = 0)
%    start = (optional) structure containing starting values: 
%            defaults: max likelihood beta, sige, V= ones(n,1)
%            start.b    = beta starting values (nvar x 1)
%            start.sige = sige starting value (1x1)
%            start.V    = V starting values (n x 1)
%---------------------------------------------------------------
% RETURNS: a structure:
%          results.meth  = 'tobit_g'
%          results.bdraw = bhat draws (ndraw-nomit x nvar)
%          results.sdraw = sige draws (ndraw-nomit x 1)
%          results.vmean = mean of vi draws (1 x nobs)
%          results.ymean = mean of y draws (1 x nobs)
%          results.rdraw = r-value draws (ndraw-nomit x 1), if Gamma(m,k) prior
%          results.pmean = b prior means 
%          results.pstd  = b prior std deviations
%          results.m     = prior m-value for r hyperparameter (if input)
%          results.k     = prior k-value for r hyperparameter (if input)
%          results.nu    = prior nu-value for sige prior
%          results.d0    = prior d0-value for sige prior
%          results.r     = value of hyperparameter r (if input)
%          results.nobs  = # of observations
%          results.nobsc = # of censored observations
%          results.nvar  = # of variables
%          results.ndraw = # of draws
%          results.nomit = # of initial draws omitted
%          results.y     = actual observations
%          results.x     = x-matrix
%          results.time  = time taken for sampling
%----------------------------------------------------------------
% NOTE: use either improper prior.rval 
%       or informative Gamma prior.m, prior.k, not both of them
%----------------------------------------------------------------
% References: Siddhartha Chib
%             Bayes Inference in the Tobit censored regression model
%            J. Econometrics Vol. 51, 1992, pp. 79-100.
%----------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[n k] = size(x);   

   
if nargin == 6   % user-supplied starting values
    if ~isstruct(start)
    error('tobit_g: must supply starting values in a structure');
     end;
    if ~isstruct(prior)
    error('tobit_g: must supply the prior as a structure variable');
    end;        
sflag = 1; tflag = 0; vflag = 0;
b0 = start.b; sige = start.sig; V = start.V;
    % error checking on starting values input
    [n1 n2] = size(b0); [n3 n4] = size(sige); [n7 n8] = size(V);
    if n1 ~= k
     error('tobit_g: starting beta values are wrong');
    elseif n2 ~= 1
     error('tobit_g: starting beta values are wrong');
    elseif n3 ~= 1
     error('tobit_g: starting sige value is wrong');
    elseif n4 ~= 1
     error('tobit_g: starting sige value is wrong');
    elseif n7 ~= n;
     error('tobit_g: starting V should be nobs x 1');
    elseif n8 ~= 1
     error('tobit_g: starting V should be nobs x 1');
    end;
    
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
    elseif strcmp(fields{i},'trunc');
      if strcmp(prior.trunc,'left');
      tflag = 0;
      else
      tflag = 1;
      end;
    elseif strcmp(fields{i},'limit');
        vflag = prior.limit;            
    end;
end;

elseif  nargin == 5  % probit maximum likelihood starting values

fields = fieldnames(prior);
nf = length(fields);
mm = 0; 
rval = 4;  % rval = 4 is default
nu = 0;    % default diffuse prior for sige
d0 = 0;
c = zeros(k,1);
T = eye(k)*1e+12;
vflag = 0; tflag = 0;
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
    elseif strcmp(fields{i},'trunc');
      if strcmp(prior.trunc,'left');
      tflag = 0;
      else
      tflag = 1;
      end;
    elseif strcmp(fields{i},'limit');
        vflag = prior.limit;           
    end;
end;
    
if tflag == 0    
in.trunc = 'left';
elseif tflag == 1
    in.trunc = 'right';
end;
in.limit = vflag;
resp = tobit(y,x,in);
b0 = resp.beta; 
sige = resp.sige;
V = ones(n,1); in = ones(n,1); % initial value for V  


elseif nargin == 4 % use default prior values
mm = 0; 
rval = 4;  % rval = 4 is default
nu = 0;    % default diffuse prior for sige
d0 = 0;
c = zeros(k,1);
T = eye(k)*1e+12; 
vflag = 0; tflag = 0;
resp = tobit(y,x);
b0 = resp.beta; 
sige = resp.sige;
V = ones(n,1); in = ones(n,1); % initial value for V  

else
error('Wrong # of arguments to tobit_g');
end;

% error checking on prior information inputs
[checkk,junk] = size(c);
if checkk ~= k
error('tobit_g: prior means are wrong');
elseif junk ~= 1
error('tobit_g: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('tobit_g: prior bcov is wrong');
elseif junk ~= k
error('tobit_g: prior bcov is wrong');
end;


Q = inv(T);
Qpc = Q*c;

bsave = zeros(ndraw-nomit,k);    % allocate storage for results
ymean = zeros(1,n); 
rsave = zeros(ndraw-nomit,1);
vmean = zeros(1,n);
ssave = zeros(ndraw-nomit,1);

yin = y;          % save original y-values

% find # of censored observations
if tflag == 1
   results.nobsc = length(find(y >= vflag));
else
   results.nobsc = length(find(y <= vflag));
end;

ind_left = find(yin <= vflag); 
ileft = ones(length(ind_left),1);    
ind_right = find(yin >= vflag);
iright = ones(length(ind_right),1);

hwait = waitbar(0,'MCMC sampling ...');
t0 = clock;                  
%  Start the sampling
for iter=1:ndraw;

          % update b ;
          xstar = matmul(x,sqrt(V));
          ystar = y.*sqrt(V);
          xpxi = inv(xstar'*xstar + sige*Q);
          b = xpxi*(xstar'*ystar + sige*Qpc);
          % draw MV normal with mean(b), var(b)
          beta = norm_rnd(sige*xpxi) + b;
         
          % update sige 
          nu1 = n + nu; 
          e = ystar - xstar*beta;
          d1 = d0 + e'*e;
          chi = chis_rnd(1,nu1);
          sige =d1/chi;
          
          % update V
          e = y - x*beta;
          chiv = chis_rnd(n,rval+1);   
          vi = ((e.*e./sige) + in*rval)./chiv;
          V = in./vi;  

          % update r
          if mm ~= 0
          rval = gamm_rnd(1,1,mm,kk);  % update rval
          end;
                  
         % simulate y from truncated normal 
         aa = x*b;
         if tflag == 0 % left censoring
            % simulate from truncated normal at the right
           y(ind_left,1) = normrt_rnd(aa(ind_left,1),sige*ileft,vflag*ileft);
         else % right censoring
            % simulate from truncated normal at the left
           y(ind_right,1) = normlt_rnd(aa(ind_right,1),sige*iright,vflag*iright);
         end;

if iter > nomit; % save draws
vmean = vmean + vi';            
ssave(iter-nomit,1) = sige;
bsave(iter-nomit,:) = beta';
ymean = ymean + y';
    if mm~= 0
        rsave(i-nomit,1) = rval;
    end;
end; % end of if
          
waitbar(iter/ndraw);

end;% end of for iter=1:ndraw
gtime = etime(clock,t0);
close(hwait);

vmean = vmean/(ndraw-nomit);
ymean = ymean/(ndraw-nomit);

results.meth  = 'tobit_g';
results.bdraw = bsave;
results.sdraw = ssave;
results.vmean = vmean;
results.ymean = ymean;
results.pmean = c;
results.pstd  = sqrt(diag(T));
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
results.ndraw = ndraw;
results.nomit = nomit;
results.time = gtime;
results.y = yin;
results.x = x;
results.nu = nu;
results.d0 = d0;
results.pflag = 'plevel';