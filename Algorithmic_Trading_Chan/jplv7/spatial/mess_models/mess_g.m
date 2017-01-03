function results = mess_g(y,x,options,ndraw,nomit,prior,start)
% PURPOSE: Bayesian estimates of the matrix exponential spatial model (mess)
% [based on a given value of rho and # of neighbors]
% S*y = X*b + e,           with xflag == 0, or:
% S*y = [i X D*X]*b + e,   with xflag == 1
%          e = N(0,sige*In), 
%          S = e^alpha*D
%          B = N(c,T), 
%          1/sige = Gamma(nu,d0), 
%          alpha = Uniform(amin,amax) or alpha = N(a,B) 
% D = a weight matrix constructed from neighbors N_i: using
% D = sum rho^i N_i / sum rho^i, i=1,...,#neighbors
% This function uses:
%  a single value of rho from options.rho 
%  a single value for #neighbors from options.neigh
%-------------------------------------------------------------
% USAGE: results = mess_g(y,x,options,ndraw,nomit,prior,start)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%  options = a structure variable with:
%  options.latt  = lattitude coordinates (nx1 vector)
%  options.long  = longitude coordinates (nx1 vector)
%  options.neigh = # of neighbors to use in constructing D (default = 5)
%  options.xflag = 0 for S*y = X*b + e,         model (default)
%                = 1 for S*y = [i X D*X]*b + e, model
%  options.rho   = value of rho to use in discounting 
%                  (0 < rho < 1), (default 1)
%  options.nflag = 0 for neighbors using 1st and 2nd order Delauney (default)
%                = 1 for neighbors using 3rd and 4th order Delauney
%                  for large nobs, and large # neighbors, used nflag = 1
%  options.q     = # of terms to use in the matrix exponential
%                  expansion (default = 7)
%    ndraw = # of draws
%    nomit = # of initial draws omitted for burn-in            
%    prior = a structure variable with:
%            prior.beta  = prior means for beta,   c above (default 0)
%                          (nvar x 1 vector, D*X terms have diffuse prior =0)
%            priov.bcov  = prior beta covariance , T above (default 1e+12)
%                          (nvar x nvar matrix, D*X terms have diffuse prior)
%            prior.alpha = prior mean for alpha    a above (default uniform)
%            prior.rcov  = prior alpha variance,   B above 
%            prior.nu    = informative Gamma(nu,d0) prior on sige
%            prior.d0    = default: nu=0,d0=0 (diffuse prior)
%            prior.m,    = informative Gamma(m,k) prior on r
%            prior.k,    = informative Gamma(m,k) prior on r
%            prior.amin  = (optional) min alpha used in sampling 
%                          (default = max like alpha - 3*std(alpha))
%            prior.amax  = (optional) max alpha used in sampling 
%                          (default = max like alpha + 3*std(alpha))
%            prior.lflag = 0 (default for marginal likelihood calculations)
%                        = 1 for no marginal likelihood (faster)
%    start = (optional) structure containing starting values: 
%            defaults: beta=ones(k,1),sige=1,rho=0.5, V=ones(n,1)
%            start.b   = beta starting values (nvar x 1)
%            start.a   = alpha starting value (scalar)
%            start.sig = sige starting value  (scalar)
%-------------------------------------------------------------
% RETURNS:  a structure:
%          results.meth   = 'mess_g'
%          results.bdraw  = bhat draws (ndraw-nomit x nvar)
%          results.bmean  = mean of bhat draws
%          results.bstd   = std of bhat draws
%          results.adraw  = alpha draws (ndraw-nomit x 1)
%          results.amean  = mean of alpha draws
%          results.astd   = std of alpha draws
%          results.sdraw  = sige draws (ndraw-nomit x 1)
%          results.smean  = mean of sige draws
%          results.lmean  = marginal likelihood based on mean of draws
%          results.bprior = b prior means, prior.beta from input
%          results.bpstd  = b prior std deviations sqrt(diag(prior.bcov))
%          results.nobs   = # of observations
%          results.nvar   = # of variables in x-matrix (plus D*X matrix)
%          results.ndraw  = # of draws
%          results.nomit  = # of initial draws omitted
%          results.y      = y-vector from input (nobs x 1)
%          results.yhat   = mean of posterior predicted (nobs x 1)
%          results.nu     = nu prior parameter
%          results.d0     = d0 prior parameter
%          results.stime  = time for sampling
%          results.time   = total time taken  
%          results.ntime  = time taken for mesh over rho and alpha values
%          results.accept = acceptance rate 
%          results.amax   = amax: max like alpha + 2*std(alpha) (or user input)
%          results.amin   = amin: max like alpha - 2*std(alpha) (or user input)      
%          results.rho    = rho from user input
%          results.tflag  = 'plevel' (default) for printing p-levels
%                         = 'tstat' for printing bogus t-statistics 
%          results.palpha = prior for alpha (from input)
%          results.acov   = prior variance for alpha (from input)
%          results.pflag  = 1, if a normal(a,B) prior for alpha, 0 otherwise
%          results.xflag  = model flag from input
%          results.neigh  = # of terms in flexible D-matrix specification
%                          (from input or default)
%          results.q      = q value from input (or default)
% --------------------------------------------------------------
% NOTES: 1) if the model includes a constant term
% it should be entered as the first column in the x-matrix
% that is input to the function
% 1) mess_g1 produces a posterior distribution for # neighbors
% 2) mess_g2 produces a posterior distribution for the hyperparameter rho
% 3) mess_g3 produces posteriors for both rho and # of neighbors
% --------------------------------------------------------------
% SEE ALSO: mess_gd, messv_g, prt, mess
% --------------------------------------------------------------
% REFERENCES: LeSage and Pace (2000) "Bayesian Estimation of the
% Matrix Exponential Spatial Specification", unpublished manuscript
%----------------------------------------------------------------

% written by:
% James P. LeSage, 1/2000
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


timet = clock;

% error checking on inputs
[n junk] = size(y);
results.y = y;
[n1 k] = size(x);

if n1 ~= n
error('mess_g: x-matrix contains wrong # of observations');
end;

% set defaults
q = 7;
xflag = 0;
nflag = 0;
rho = 1;
neigh = 5;
aflag = 0;
llflag = 0;
pflag = 0; % flag for the presence or absent of a prior on alpha

mm = 0;    % set defaults
nu = 0;    % default diffuse prior for sige
d0 = 0;
sig0 = 1;         % default starting values for sige
astart = -1;      % default starting value for alpha
c = zeros(k,1);   % diffuse prior for beta
T = eye(k)*1e+12;
palpha = -1;
S = 1e+12;
lflag = 0; % default to do marginal likelihood calculations
if nargin == 7
    if ~isstruct(start)
        error('mess_g: must supply starting values in a structure');
    end;
 % parse starting values entered by the user
 fields = fieldnames(start);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'b')
        b0 = start.b; 
        [n1 n2] = size(b0); % error checking on user inputs
       if n1 ~= k
        error('mess_g: starting beta values are wrong');
       elseif n2 ~= 1
        error('mess_g: starting beta values are wrong');
       end;
    elseif strcmp(fields{i},'sig')
        sig0 = start.sig;
       [n1 n2] = size(sig0); % error checking on user inputs
       if n1 ~= 1
        error('mess_g: starting sige value is wrong');
       elseif n2 ~= 1
        error('mess_g: starting sige value is wrong');
       end;
    elseif strcmp(fields{i},'a')
        astart = start.a;
       [n1 n2] = size(astart); % error checking on user inputs
       if n1 ~= 1
        error('mess_g: starting alpha value is wrong');
       elseif n2 ~= 1
        error('mess_g: starting alpha value is wrong');
       end;
    end;
 end; % end of for loop
% parse options structure
    if ~isstruct(options)
        error('mess_g: must supply option values in a structure');
    end;
 fields = fieldnames(options);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'xflag')
       xflag = options.xflag;
    elseif strcmp(fields{i},'rho')
        rho = options.rho;
    elseif strcmp(fields{i},'q')
       q = options.q;
     elseif strcmp(fields{i},'neigh')
       neigh = options.neigh;
     elseif strcmp(fields{i},'latt')
        latt = options.latt; llflag = llflag + 1;
     elseif strcmp(fields{i},'long')
        long = options.long; llflag = llflag + 1;
    elseif strcmp(fields{i},'nflag')
        nflag = options.nflag;
     end;
 end; % end of for loop

% parse prior structure variable inputs        
    if ~isstruct(prior)
    error('mess_g: must supply the prior as a structure variable');
    end;

fields = fieldnames(prior);
nf = length(fields);

for i=1:nf
    if strcmp(fields{i},'beta')
        c = prior.beta;
    elseif strcmp(fields{i},'bcov')
        T = prior.bcov;
    elseif strcmp(fields{i},'alpha')
        palpha = prior.alpha; pflag = 1;
    elseif strcmp(fields{i},'acov')
        S = prior.acov;        
    elseif strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;
    elseif strcmp(fields{i},'lflag')
       lflag = prior.lflag; 
    end;
end;


elseif nargin == 6   % we supply default starting values
 fields = fieldnames(prior);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'beta')
        c = prior.beta;
    elseif strcmp(fields{i},'bcov')
        T = prior.bcov;
    elseif strcmp(fields{i},'alpha')
        palpha = prior.alpha; pflag = 1;
    elseif strcmp(fields{i},'acov')
        S = prior.acov;         
    elseif strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;
    elseif strcmp(fields{i},'rval')
       rval = prior.rval; 
    elseif strcmp(fields{i},'lflag')
       lflag = prior.lflag; 
    end;
 end;
 
 % parse options
     if ~isstruct(options)
        error('mess_g: must supply option values in a structure');
    end;
 fields = fieldnames(options);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'xflag')
       xflag = options.xflag;
    elseif strcmp(fields{i},'rho')
        rho = options.rho;
    elseif strcmp(fields{i},'q')
       q = options.q;
     elseif strcmp(fields{i},'neigh')
       neigh = options.neigh;
     elseif strcmp(fields{i},'latt')
        latt = options.latt; llflag = llflag + 1;
     elseif strcmp(fields{i},'long')
        long = options.long; llflag = llflag + 1;
    elseif strcmp(fields{i},'nflag')
        nflag = options.nflag;
     end;
 end; % end of for loop



elseif nargin == 5   % we supply all defaults
   % parse options structure
    if ~isstruct(options)
        error('mess_g: must supply option values in a structure');
    end;
 fields = fieldnames(options);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'xflag')
       xflag = options.xflag;
    elseif strcmp(fields{i},'rho')
        rho = options.rho;
    elseif strcmp(fields{i},'q')
       q = options.q;
     elseif strcmp(fields{i},'neigh')
       neigh = options.neigh;
     elseif strcmp(fields{i},'latt')
        latt = options.latt; llflag = llflag + 1;
     elseif strcmp(fields{i},'long')
        long = options.long; llflag = llflag + 1;
    elseif strcmp(fields{i},'nflag')
        nflag = options.nflag;
     end;
 end; % end of for loop

else
error('Wrong # of arguments to mess_g');
end;
      

% error checking on prior information inputs
[checkk,junk] = size(c);
if checkk ~= k
error('mess_g: prior means are wrong');
elseif junk ~= 1
error('mess_g: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('mess_g: prior bcov is wrong');
elseif junk ~= k
error('mess_g: prior bcov is wrong');
end;

[checkk junk] = size(palpha);
if checkk ~= 1
error('mess_g: prior alpha is wrong');
elseif junk ~= 1
error('mess_g: prior alpha is wrong');
end;

[checkk junk] = size(S);
if checkk ~= 1
error('mess_g: prior acov is wrong');
elseif junk ~= 1
error('mess_g: prior acov is wrong');
end;

% make sure the user input latt, long or we really bomb
if llflag ~= 2;
error('mess_g: no lattitude-longitude coordinates input');
end;

switch xflag % switch on x transformation

   
   case{0} % case where x variables are not transformed


% ====== initializations
% compute this stuff once to save time
TI = inv(T);
TIc = TI*c;

% ========= do up front grid over rho, alpha values

t1 = clock;   % time this operation

% find index into nearest neighbors
if nflag == 0
nnlist = find_nn(latt,long,neigh);
elseif nflag == 1
nnlist = find_nn2(latt,long,neigh);
else
error('mess_g1: bad nflag option');
end;

% check for empty nnlist columns
chk = find(nnlist == 0);
if length(chk) > 0; 
 if nflag == 1 % no saving the user here
 error('mess_g3: trying too many neighbors, some do not exist');
 else % we save the user here
 nnlist = find_nn2(latt,long,neigh);
 end;
end;

tmp = rho.^(0:neigh-1);
tmp = tmp/sum(tmp);

% construct and save Sy
wy = y;
Y = y(:,ones(1,q));
for i=2:q;
wy = wy(nnlist)*tmp';
Y(:,i) = wy;
end;

% end of up front stuff with Sy saved in Symat
gtime = etime(clock,t1);

% initializations and starting values for the sampler
alpha = astart;
cc=0.2;   % initial metropolis value
cnta = 0; % counter for acceptance rate for alpha
iter = 1;
in = ones(n,1);
sige = sig0;

% storage for draws
          bsave = zeros(ndraw-nomit,k);
          asave = zeros(ndraw-nomit,1);
          ssave = zeros(ndraw-nomit,1);
           lsave = 0;
          rtmp = zeros(nomit,1);

hwait = waitbar(0,'MCMC sampling ...');
t0 = clock;                  
iter = 1;
          while (iter <= ndraw); % start sampling;

          [junk nq] = size(Y);
          nq1 = nq-1;
          v = ones(nq,1);
          for i=2:nq;
          v(i,1) = alpha.^(i-1);
          end;
          W = (1./[1 cumprod(1:nq1)]);
          Sy = Y*diag(W)*v;

          % update beta   
          AI = inv(x'*x + sige*TI);    
          b = x'*Sy + sige*TIc;
          b0 = AI*b;
          bhat = norm_rnd(sige*AI) + b0;  
         
          % update sige
          nu1 = n + 2*nu; 
          e = (Sy - x*bhat);
          d1 = 2*d0 + e'*e;
          chi = chis_rnd(1,nu1);
          sige = d1/chi;
                       
          % metropolis step to get alpha update
          if pflag == 0
          alphax = c_mess(alpha,y,x,Y,bhat,sige); 
          elseif pflag == 1
          alphax = c_mess(alpha,y,x,Y,bhat,sige,palpha,S); 
          end;
          
          accept = 0; 
          alpha2 = alpha + cc*randn(1,1);
          while accept == 0 
           if alpha2 <= 0 
           accept = 1;  
           else
           alpha2 = alpha + cc*randn(1,1);
           cnta = cnta+1; % counts accept rate for alpha
           end; 
          end; 
          if pflag == 0
           alphay = c_mess(alpha2,y,x,Y,bhat,sige);
          elseif pflag == 1
           alphay = c_mess(alpha2,y,x,Y,bhat,sige,palpha,S);
          end;
          
          ru = unif_rnd(1,0,1);
          if ((alphay - alphax) > exp(1)),
          p = 1;
          else,          
          ratio = exp(alphay-alphax);
          p = min(1,ratio);
          end;
              if (ru < p)
                 alpha = alpha2;
              end;

          rtmp(iter,1) = alpha;

      
     % evaulate the likelihood using current draws
     if lflag == 0
                like = -(n/2)*log(2*pi*sige) - (e'*e)/(2*sige);
     end;
                       
     % update rval
     if mm ~= 0           
     rval = gamm_rnd(1,1,mm,kk);  
     end;
              
    if iter > nomit % if we are past burn-in, save the draws
    bsave(iter-nomit,:) = bhat';
    ssave(iter-nomit,1) = sige;
    asave(iter-nomit,1) = alpha; 
     if lflag == 0
       lsave = lsave + like;
     else
       lsave = lsave + 0;
     end;
    end;
                    
    if iter == nomit % update cc based on initial draws
         tst = 2*std(rtmp(1:nomit,1));
         if tst > 0.1
         cc = tst;
         end;
    end;

iter = iter + 1; 
waitbar(iter/ndraw);         
end; % end of sampling loop
close(hwait);

stime = etime(clock,t0);

% compute posterior means
if lflag == 0
lmean = lsave/(ndraw-nomit);
else 
   lmean = 0;
end;
amean = mean(asave);
bmean = mean(bsave);
astd = std(asave);
bstd = std(bsave);
smean = mean(ssave);

% find acceptance rate
results.accept = 1 - cnta/(iter+cnta);
% NOTE: this could be interpreted as the
% probability that alpha is in the mesh grid

% do the expensive calculation here
% rather than lookup
tmp = rho.^(0:neigh-1);
tmp = tmp/sum(tmp);

wy = y;
Y = y(:,ones(1,q));
for i=2:q;
wy = wy(nnlist)*tmp';
Y(:,i) = wy;
end;
[junk nq] = size(Y);
nq1 = nq-1;
v = ones(nq,1);
for i=2:nq;
v(i,1) = amean.^(i-1);
end;
W = (1./[1 cumprod(1:nq1)]);
sy = Y*diag(W)*v;

e = sy - x*bmean';
yhat = y - e;

sigu = e'*e;
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(n-k);
rsqr2 = rsqr2/(n-1.0);
rbar = 1 - (rsqr1/rsqr2); % rbar-squared

time = etime(clock,timet);


results.meth  = 'mess_g';
results.bdraw = bsave;
results.adraw = asave;
results.bmean = bmean';
results.bstd  = bstd';
results.amean = amean;
results.astd  = astd;
results.smean = smean;
results.sdraw = ssave;
results.rho   = rho;
results.lmean = lmean;
results.bprior = c;
results.bpstd  = sqrt(diag(T));
results.nobs  = n;
results.nvar  = k;
results.ndraw = ndraw;
results.nomit = nomit;
results.time  = time;
results.stime = stime;
results.ntime = gtime;
results.nu = nu;
results.d0 = d0;
results.tflag = 'plevel';
results.aflag = pflag;
results.palpha = palpha;
results.acov = S;
results.y = y;
results.yhat = yhat;
results.resid = e;
results.rsqr = rsqr;
results.rbar = rbar;
results.neigh = neigh;
results.q     = q;
results.nobs = n;
results.nvar = k;
results.xflag = xflag;
results.nflag = nflag;

case{1} % case of x-variables transformed
   
   xone = x(:,1);
   if all(xone == 1)
      xsub = x(:,2:k);
   else
      xsub = x;
   end;

t1 = clock;   % time this operation

tmp = rho.^(0:neigh-1);
tmp = tmp/sum(tmp);

% we have to construct the weight matrix using neighbors
% find index into nearest neighbors
if nflag == 0
nnlist = find_nn(latt,long,neigh);
elseif nflag == 1
nnlist = find_nn2(latt,long,neigh);
else
error('mess_g: bad nflag option');
end;

% check for empty nnlist columns
chk = find(nnlist == 0);
if length(chk) > 0; 
 if nflag == 1 % no saving the user here
 error('mess_g: trying too many neighbors, some do not exist');
 else % we save the user here
 nnlist = find_nn2(latt,long,neigh);
 end;
end;

% construct and save Sy
wy = y;
Y = y(:,ones(1,q));
for i=2:q;
wy = wy(nnlist)*tmp';
Y(:,i) = wy;
end;

% create and save Sx
[junk nk] = size(xsub);
xout = x;
for i=1:nk;
xi = xsub(:,i);
tmpp = xi(nnlist)*tmp';
xout = [xout tmpp];
end;
xmat = xout;

% end of up front stuff with Sy saved 
gtime = etime(clock,t1);

% ====== initializations
% compute this stuff once to save time
[junk kk] = size([x xsub]); % need to add diffuse priors 
                          % to the spatial lags of x-variables
Tnew = eye(kk)*1e+12;
Tnew(1:k,1:k) = T;
TI = inv(Tnew);
ctmp = zeros(kk,1);
ctmp(1:k,1) = c;
c = ctmp;
TIc = TI*c;
cc=0.2; % initial metropolis value
cntr = 0; 
iter = 1;
alpha = astart;
rho = 1;
in = ones(n,1);
sige = sig0;

% storage for draws
          bsave = zeros(ndraw-nomit,kk);
          asave = zeros(ndraw-nomit,1);
          ssave = zeros(ndraw-nomit,1);
           lsave = 0;
          rtmp = zeros(nomit,1);



hwait = waitbar(0,'MCMC sampling ...');
t0 = clock;                  
iter = 1;
          while (iter <= ndraw); % start sampling;

          [junk nq] = size(Y);
          nq1 = nq-1;
          v = ones(nq,1);
          for i=2:nq;
          v(i,1) = alpha.^(i-1);
          end;
          W = (1./[1 cumprod(1:nq1)]);
          Sy = Y*diag(W)*v;

          % update beta   
          AI = inv(xmat'*xmat + sige*TI);
          
          b = xmat'*Sy + sige*TIc;
          b0 = AI*b;
          bhat = norm_rnd(sige*AI) + b0;  
         
          % update sige
          nu1 = n + 2*nu; 
          e = (Sy - xmat*bhat);
          d1 = 2*d0 + e'*e;
          chi = chis_rnd(1,nu1);
          sige = d1/chi;

          % metropolis step to get alpha update
          if pflag == 0
          alphax = c_mess(alpha,y,xmat,Y,bhat,sige); 
          elseif pflag == 1
          alphax = c_mess(alpha,y,xmat,Y,bhat,sige,palpha,S); 
          end;
          
          accept = 0;
          alpha2 = alpha + cc*randn(1,1);
          while accept == 0
           if alpha2 <=0 
           accept = 1;  
           else
           alpha2 = alpha + cc*randn(1,1);
           cntr = cntr+1; % counts accept rate
           end; 
          end; 
           if pflag == 0
           alphay = c_mess(alpha2,y,xmat,Y,bhat,sige);
           elseif pflag == 1
           alphay = c_mess(alpha2,y,xmat,Y,bhat,sige,palpha,S);
           end;
          
          ru = unif_rnd(1,0,1);
          if ((alphay - alphax) > exp(1)),
          p = 1;
          else,          
          ratio = exp(alphay-alphax);
          p = min(1,ratio);
          end;
              if (ru < p)
                 alpha = alpha2;
              end;

          rtmp(iter,1) = alpha;
      
     % evaulate the likelihood using current draws
     if lflag == 0
                like = -(n/2)*log(2*pi*sige) - (e'*e)/(2*sige);
     end;
                       
     % update rval
     if mm ~= 0           
     rval = gamm_rnd(1,1,mm,kk);  
     end;
              
    if iter > nomit % if we are past burn-in, save the draws
    bsave(iter-nomit,:) = bhat';
    ssave(iter-nomit,1) = sige;
    asave(iter-nomit,1) = alpha; 
    if lflag == 0
       lsave = lsave + like;
    else
       lsave = lsave + 0;
    end;
    
    
    end;

       if iter == nomit % update cc based on initial draws
         tst = 2*std(rtmp(1:nomit,1));
         if tst > 0.05
         cc = tst;
         end;
    end;
                 

iter = iter + 1; 
waitbar(iter/ndraw);         
end; % end of sampling loop
close(hwait);

stime = etime(clock,t0);

% compute posterior means
if lflag == 0
lmean = lsave/(ndraw-nomit);
else 
   lmean = 0;
end;
amean = mean(asave);
bmean = mean(bsave);
astd = std(asave);
bstd = std(bsave);
smean = mean(ssave);

% find acceptance rate
results.accept = 1 - cntr/(iter+cntr);

tmp = rho.^(0:neigh-1);
tmp = tmp/sum(tmp);

% construct Sy, Sx based on posterior means
wy = y;
Y = y(:,ones(1,q));
for i=2:q;
wy = wy(nnlist)*tmp';
Y(:,i) = wy;
end;
[junk nq] = size(Y);
nq1 = nq-1;
v = ones(nq,1);
for i=2:nq;
v(i,1) = amean.^(i-1);
end;
W = (1./[1 cumprod(1:nq1)]);
sy = Y*diag(W)*v;

% create  Sx based on posterior mean of rho
xmat = x;
for i=1:nk;
xi = xsub(:,i);
tmpp = xi(nnlist)*tmp';
xmat = [xmat tmpp];
end;

e = Sy - xmat*bmean';
yhat = y - e;

sigu = e'*e;
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(n-k);
rsqr2 = rsqr2/(n-1.0);
rbar = 1 - (rsqr1/rsqr2); % rbar-squared

time = etime(clock,timet);


results.meth  = 'mess_g';
results.bdraw = bsave;
results.adraw = asave;
results.bmean = bmean';
results.bstd  = bstd';
results.amean = amean;
results.astd  = astd;
results.smean = smean;
results.sdraw = ssave;
results.lmean = lmean;
results.bprior = c;
results.bpstd  = sqrt(diag(Tnew));
results.nobs  = n;
results.nvar  = k;
results.ndraw = ndraw;
results.nomit = nomit;
results.time  = time;
results.stime = stime;
results.nu = nu;
results.d0 = d0;
results.tflag = 'plevel';
results.aflag = aflag;
results.palpha = palpha;
results.acov = S;
results.y = y;
results.yhat = yhat;
results.resid = e;
results.rsqr = rsqr;
results.rbar = rbar;
results.rho   = rho;
results.neigh = neigh;
results.q     = q;
results.ntime = gtime;
results.nobs = n;
results.nvar = k;
results.xflag = xflag;


otherwise
   
end; % end of switch


   
function cout = c_mess(alpha,ys,xs,Symat,beta,sige,a,B);
% PURPOSE: evaluate the conditional distribution of alpha 
%          for the Bayesian mess_g model
% ---------------------------------------------------
%  USAGE: cout = c_mess2(alpha,y,x,Symat,beta,alpha,rho)
%  where:  alpha  = matrix exponential alpha spatial parameter
%            y    = dependent variable vector
%            x    = explanatory variables matrix
%          Symat  = matrix from mess_g 
%            beta = kx1 current bhat vector
%            sige = current value of sige
%               a = prior mean for alpha     (optional input)
%               B = prior variance for alpha (optional input)
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by mess_g
%  --------------------------------------------------
%  SEE ALSO: mess_g, mess_gd
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


n = length(ys);

[junk nq] = size(Symat);
nq1 = nq-1;
v = ones(nq,1);
for i=2:nq;
v(i,1) = alpha.^(i-1);
end;
W = (1./[1 cumprod(1:nq1)]);
Sy = Symat*diag(W)*v;

e = Sy - xs*beta;
epe = e'*e;

if nargin == 6
cout =  -epe/(2*sige);
elseif nargin == 8
B = B*sige;
cout = -epe/(2*sige) - 0.5*(((alpha-a)^2)/B);
else
error('c_mess: Wrong # of inputs arguments');
end;
