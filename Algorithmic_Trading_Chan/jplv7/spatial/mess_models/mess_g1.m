function results = mess_g1(y,x,options,ndraw,nomit,prior,start)
% PURPOSE: Bayesian estimates of the matrix exponential spatial model (mess)
% % [samples values of neighbors to produce a posterior distribution]
% S*y = X*b + e,           with xflag == 0, or:
% S*y = [i X D*X]*b + e,   with xflag == 1
%          e = N(0,sige*In), 
%          S = e^alpha*D
%          B = N(c,T),            (default: diffuse)
%          1/sige = Gamma(nu,d0), (default: diffuse)
%          alpha =  N(a,B),       (default: diffuse) 
% D = a weight matrix constructed from neighbors N_i using:
% D = sum rho^i N_i / sum rho^i, i=1,...,#neighbors
% NOTE: mess_g()  uses fixed #neighbors, fixed rho 
%       mess_g1() estimates #neighbors, fixed rho
%       mess_g2() uses fixed #neighbors, estimates rho
%       mess_g3() estimates #neighbors and rho
%-------------------------------------------------------------
% USAGE: results = mess_g1(y,x,options,ndraw,nomit,prior,start)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%  options = a structure variable with:
%  options.latt  = lattitude coordinates (nx1 vector)
%  options.long  = longitude coordinates (nx1 vector)
%  options.rho = rho value to use in constructing D (0 < rho < 1), (default 1)
%  options.xflag = 0 for S*y = X*b + e,         model (default)
%                = 1 for S*y = [i X D*X]*b + e, model
%  options.mmin  = minimum # neighbors to use (default = 4)                  
%  options.mmax  = maximum # neighbors to use (default = 10)
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
%    start = (optional) structure containing starting values: 
%            defaults: beta=ones(k,1),sige=1,rho=0.5, V=ones(n,1)
%            start.b   = beta starting values (nvar x 1)
%            start.a   = alpha starting value (scalar)
%            start.sig = sige starting value  (scalar)
%-------------------------------------------------------------
% RETURNS:  a structure:
%          results.meth   = 'mess_g1'
%          results.bdraw  = bhat draws (ndraw-nomit x nvar)
%          results.bmean  = mean of bhat draws
%          results.bstd   = std of bhat draws
%          results.adraw  = alpha draws (ndraw-nomit x 1)
%          results.amean  = mean of alpha draws
%          results.astd   = std of alpha draws
%          results.sdraw  = sige draws (ndraw-nomit x 1)
%          results.smean  = mean of sige draws
%          results.lmean  = marginal likelihood based on mean of draws
%          results.mmean  = posterior mean of # neighbors
%          results.mstd   = posterior std  of # neighbors
%          results.mdraw  = draws for # neighbors
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
%          results.mmax   = mmax: default (or user input)
%          results.mmin   = mmin: default (or user input)        
%          results.tflag  = 'plevel' (default) for printing p-levels
%                         = 'tstat' for printing bogus t-statistics 
%          results.palpha = prior for alpha (from input)
%          results.acov   = prior variance for alpha (from input)
%          results.pflag  = 1, if a normal(a,B) prior for alpha, 0 otherwise
%          results.xflag  = model flag from input
%          results.rho    = rho value used (from input or default)
%          results.q      = q value from input (or default)

% --------------------------------------------------------------
% NOTES: if the model includes a constant term
% it should be entered as the first column in the x-matrix
% that is input to the function
% 1) mess_g1 produces a posterior distribution for # neighbors
% 2) mess_g2 produces a posterior distribution for the hyperparameter rho
% 3) mess_g3 produces posteriors for both rho and # of neighbors
% --------------------------------------------------------------
% SEE ALSO:  mess_g1d, mess_g, mess_g2, mess_g3, prt, mess
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
error('mess_g1: x-matrix contains wrong # of observations');
end;

% set defaults
q = 7;
xflag = 0;
nflag = 0;
mmin = 4;
mmax = 10;
rho = 1;
nflag = 0;
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
        error('mess_g1: must supply starting values in a structure');
    end;
 % parse starting values entered by the user
 fields = fieldnames(start);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'b')
        b0 = start.b; 
        [n1 n2] = size(b0); % error checking on user inputs
       if n1 ~= k
        error('mess_g1: starting beta values are wrong');
       elseif n2 ~= 1
        error('mess_g1: starting beta values are wrong');
       end;
    elseif strcmp(fields{i},'sig')
        sig0 = start.sig;
       [n1 n2] = size(sig0); % error checking on user inputs
       if n1 ~= 1
        error('mess_g1: starting sige value is wrong');
       elseif n2 ~= 1
        error('mess_g1: starting sige value is wrong');
       end;
    elseif strcmp(fields{i},'a')
        astart = start.a;
       [n1 n2] = size(astart); % error checking on user inputs
       if n1 ~= 1
        error('mess_g1: starting alpha value is wrong');
       elseif n2 ~= 1
        error('mess_g1: starting alpha value is wrong');
       end;
    end;
 end; % end of for loop
% parse options structure
    if ~isstruct(options)
        error('mess_g1: must supply option values in a structure');
    end;
 fields = fieldnames(options);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'xflag')
       xflag = options.xflag;
    elseif strcmp(fields{i},'mmin')
        mmin = options.mmin;
    elseif strcmp(fields{i},'mmax')
        mmax = options.mmax;
    elseif strcmp(fields{i},'q')
       q = options.q;
     elseif strcmp(fields{i},'rho')
       rho = options.rho;
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
    error('mess_g1: must supply the prior as a structure variable');
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
        error('mess_g1: must supply option values in a structure');
    end;
 fields = fieldnames(options);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'xflag')
       xflag = options.xflag;
    elseif strcmp(fields{i},'mmin')
        mmin = options.mmin;
    elseif strcmp(fields{i},'mmax')
        mmax = options.mmax;
    elseif strcmp(fields{i},'q')
       q = options.q;
     elseif strcmp(fields{i},'rho')
       rho = options.rho;
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
        error('mess_g1: must supply option values in a structure');
    end;
 fields = fieldnames(options);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'xflag')
       xflag = options.xflag;
    elseif strcmp(fields{i},'mmin')
        mmin = options.mmin;
    elseif strcmp(fields{i},'mmax')
        mmax = options.mmax;
    elseif strcmp(fields{i},'q')
       q = options.q;
     elseif strcmp(fields{i},'rho')
       rho = options.rho;
     elseif strcmp(fields{i},'latt')
        latt = options.latt; llflag = llflag + 1;
     elseif strcmp(fields{i},'long')
        long = options.long; llflag = llflag + 1;
    elseif strcmp(fields{i},'nflag')
        nflag = options.nflag;
     end;
 end; % end of for loop

else
error('Wrong # of arguments to mess_g1');
end;
      

% error checking on prior information inputs
[checkk,junk] = size(c);
if checkk ~= k
error('mess_g1: prior means are wrong');
elseif junk ~= 1
error('mess_g1: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('mess_g1: prior bcov is wrong');
elseif junk ~= k
error('mess_g1: prior bcov is wrong');
end;

if pflag == 1
[checkk junk] = size(palpha);
if checkk ~= 1
error('mess_g1: prior alpha is wrong');
elseif junk ~= 1
error('mess_g1: prior alpha is wrong');
end;

[checkk junk] = size(S);
if checkk ~= 1
error('mess_g1: prior acov is wrong');
elseif junk ~= 1
error('mess_g1: prior acov is wrong');
end;
end;

% make sure the user input latt, long or we really bomb
if llflag ~= 2;
error('mess_g1: no lattitude-longitude coordinates input');
end;

switch xflag % switch on x transformation

   
   case{0} % case where x variables are not transformed


% ====== initializations
% compute this stuff once to save time
TI = inv(T);
TIc = TI*c;

% ========= do up front grid over neighbors

results.mmin = mmin;
results.mmax = mmax;

mgrid = mmin:1:mmax;
nneigh = length(mgrid);

t1 = clock;   % time this operation

% storage for Y over the grid
Symat = zeros(n,q,nneigh); % matrices Y for various neigh values
mmat = zeros(nneigh,1);  % save rho values

% find index into nearest neighbors
if nflag == 0
nnlistall = find_nn(latt,long,mmax);
elseif nflag == 1
nnlistall = find_nn(latt,long,mmax,3);
else
error('mess_g1: bad nflag option');
end;

% check for empty nnlist columns
chk = find(nnlistall == 0);
if length(chk) > 0; 
 if nflag == 1 % no saving the user here
 error('mess_g3: trying too many neighbors, some do not exist');
 else % we save the user here
 nnlistall = find_nn(latt,long,mmax,3);
 end;
end;


for jj=1:nneigh;
neigh = mgrid(jj);

nnlist = nnlistall(:,1:neigh);

tmp = rho.^(0:neigh-1);
tmp = tmp/sum(tmp);

% construct and save Sy
wy = y;
Y = y(:,ones(1,q));
for i=2:q;
wy = wy(nnlist)*tmp';
Y(:,i) = wy;
end;

% save Y
Symat(:,:,jj) = Y;
% save alpha
mmat(jj,1) = neigh;

end; % end of loop over neighbor values 

% end of up front stuff with Sy saved in Symat
gtime = etime(clock,t1);

% initializations and starting values for the sampler
rho = 1;
alpha = astart;
cc=0.2;   % initial metropolis value
cnta = 0; % counter for acceptance rate for alpha
cntr = 0; % counter for acceptance rate for rho
iter = 1;
in = ones(n,1);
sige = sig0;

% storage for draws
          bsave = zeros(ndraw-nomit,k);
          asave = zeros(ndraw-nomit,1);
          ssave = zeros(ndraw-nomit,1);
          msave = zeros(ndraw-nomit,1);
	       lsave = 0;
          rtmp = zeros(nomit,1);

hwait = waitbar(0,'MCMC sampling ...');
t0 = clock;                  
iter = 1;
          while (iter <= ndraw); % start sampling;

          % update beta   
          AI = inv(x'*x + sige*TI);
          
          % lookup Sy based on neigh values
          gsize = mmat(2,1) - mmat(1,1);
          i1 = find(mmat <= rho + gsize);
          i2 = find(mmat <= rho - gsize);
          i1 = max(i1);
          i2 = max(i2);
          indexm = round((i1+i2)/2);
          if isempty(indexm)
          indexm = 1;
          end;

          Ycap = squeeze(Symat(:,:,indexm));
          % create Sy based on Y
          [junk nq] = size(Ycap);
          nq1 = nq-1;
          v = ones(nq,1);
          for i=2:nq;
          v(i,1) = alpha.^(i-1);
          end;
          W = (1./[1 cumprod(1:nq1)]);
          Sy = Ycap*diag(W)*v;

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
          alphax = c_mess1(alpha,y,x,Symat,bhat,sige,neigh,mmat); 
          elseif pflag == 1
          alphax = c_mess1(alpha,y,x,Symat,bhat,sige,neigh,mmat,palpha,S); 
          end;
          
          accept = 0; 
          alpha2 = alpha + cc*randn(1,1);
          while accept == 0 
           if (alpha2 <= 0)
           accept = 1;  
           else
           alpha2 = alpha + cc*randn(1,1);
           cnta = cnta+1; % counts accept rate for alpha
           end; 
          end; 
           if pflag == 0
           alphay = c_mess1(alpha2,y,x,Symat,bhat,sige,neigh,mmat);
           elseif pflag == 1
           alphay = c_mess1(alpha2,y,x,Symat,bhat,sige,neigh,mmat,palpha,S);
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

% update neigh using metroplis-hastings step
          neighx = m_mess1(neigh,y,x,Symat,bhat,alpha,mmat); 
           accept = 0; 
          neigh2 = round(unif_rnd(1,mmin,mmax));

          neighy = m_mess1(neigh2,y,x,Symat,bhat,alpha,mmat);
           ru = unif_rnd(1,0,1);
          if ((neighy - neighx) > exp(1)),
          p = 1;
          else,          
          ratio = exp(neighy-neighx);
          p = min(1,ratio);
          end;
              if (ru < p)
                 neigh = neigh2;
              end;
	  
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
    msave(iter-nomit,1) = neigh;
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
mmean = mean(msave);
mstd = std(msave);

% find acceptance rate
results.accept = 1 - cnta/(iter+cnta);
% NOTE: this could be interpreted as the
% probability that alpha is in the mesh grid

% do the expensive calculation here
% rather than lookup
mround = round(mmean);
tmp = rho.^(0:mround-1);
tmp = tmp/sum(tmp);
nnlist = nnlistall(:,1:mround);
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


results.meth  = 'mess_g1';
results.bdraw = bsave;
results.mdraw = msave;
results.adraw = asave;
results.bmean = bmean';
results.bstd  = bstd';
results.amean = amean;
results.astd  = astd;
results.smean = smean;
results.sdraw = ssave;
results.mmean = mmean;
results.mstd  = mstd;
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
results.rho = rho;
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

% find index into nearest neighbors
if nflag == 0
nnlistall = find_nn(latt,long,mmax);
elseif nflag == 1
nnlistall = find_nn(latt,long,mmax,3);
else
error('mess_g1: bad nflag option');
end;

% check for empty nnlist columns
chk = find(nnlistall == 0);
if length(chk) > 0; 
 if nflag == 1 % no saving the user here
 error('mess_g3: trying too many neighbors, some do not exist');
 else % we save the user here
 nnlistall = find_nn(latt,long,mmax,4);
 end;
end;

% ========= do up front grid over rho, alpha values

results.mmin = mmin;
results.mmax = mmax;

mgrid = mmin:1:mmax;
nneigh = length(mgrid);

t1 = clock;   % time this operation

% storage for Sy over the grid
Symat = zeros(n,q,nneigh); % vectors of Sy for various alpha,rho values
Sxmat = zeros(n,2*k-1,nneigh); % matrices of Sx for various alpha,rho values
mmat = zeros(nneigh,1);   % save rho values


for jj=1:nneigh;
neigh = mgrid(jj);
nnlist = nnlistall(:,1:neigh);

tmp = rho.^(0:neigh-1);
tmp = tmp/sum(tmp);

% construct and save Sy
wy = y;
Y = y(:,ones(1,q));
for i=2:q;
wy = wy(nnlist)*tmp';
Y(:,i) = wy;
end;
% save Sy
Symat(:,:,jj) = Y;
% create and save Sx
[junk nk] = size(xsub);
xout = x;
for i=1:nk;
xi = xsub(:,i);
tmpp = xi(nnlist)*tmp';
xout = [xout tmpp];
end;
Sxmat(:,:,jj) = xout;
% save alpha and neigh
mmat(jj,1) = neigh;

end; % end of loop over neigh values 

% end of up front stuff with Sy saved in Symat
gtime = etime(clock,t1);

% ====== initializations
% compute this stuff once to save time
[junk kk] = size([x xsub]); % need to add diffuse priors 
                          % to the spatial lags of x-variables
Tnew = eye(kk)*1e+12;
Tnew(1:k,1:k) = T;
TI = inv(Tnew);
tmp = zeros(kk,1);
tmp(1:k,1) = c;
c = tmp;
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
          msave = zeros(ndraw-nomit,1);
	       lsave = 0;
          rtmp = zeros(nomit,1);



hwait = waitbar(0,'MCMC sampling ...');
t0 = clock;                  
iter = 1;
          while (iter <= ndraw); % start sampling;

          % lookup Sx based on alpha, neigh values
          gsize = mmat(2,1) - mmat(1,1);
          i1 = find(mmat <= neigh + gsize);
          i2 = find(mmat <= neigh - gsize);
          i1 = max(i1);
          i2 = max(i2);
          indexm = round((i1+i2)/2);
          if isempty(indexm)
          indexm = 1;
          end;

          xmat = squeeze(Sxmat(:,:,indexm));
          % get Sy for free
          Ycap = squeeze(Symat(:,:,indexm));

          % construct Sy based on look up values
          [junk nq] = size(Ycap);
          nq1 = nq-1;
          v = ones(nq,1);
          for i=2:nq;
          v(i,1) = alpha.^(i-1);
          end;
          W = (1./[1 cumprod(1:nq1)]);
          Sy = Ycap*diag(W)*v;


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
          alphax = c_mess1(alpha,y,xmat,Symat,bhat,sige,neigh,mmat); 
          elseif pflag == 1
          alphax = c_mess1(alpha,y,xmat,Symat,bhat,sige,neigh,mmat,palpha,S); 
          end;
          
          accept = 0;
          alpha2 = alpha + cc*randn(1,1);
          while accept == 0
           if alpha2 <= 0
           accept = 1;  
           else
           alpha2 = alpha + cc*randn(1,1);
           cntr = cntr+1; % counts accept rate
           end; 
          end; 
           if pflag == 0
           alphay = c_mess1(alpha2,y,xmat,Symat,bhat,sige,neigh,mmat);
           elseif pflag == 1
           alphay = c_mess1(alpha2,y,xmat,Symat,bhat,sige,neigh,mmat,palpha,S);
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

% update neigh using metroplis-hastings step
          neighx = m_mess1(neigh,y,xmat,Symat,bhat,alpha,mmat); 
          neigh2 = round(unif_rnd(1,mmin,mmax));

          neighy = m_mess1(neigh2,y,xmat,Symat,bhat,alpha,mmat);
           ru = unif_rnd(1,0,1);
          if ((neighy - neighx) > exp(1)),
          p = 1;
          else,          
          ratio = exp(neighy-neighx);
          p = min(1,ratio);
          end;
              if (ru < p)
                 neigh = neigh2;
              end;
	  

	  
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
    msave(iter-nomit,1) = neigh;
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
mmean = mean(msave);
mstd = std(msave);

% find acceptance rate
results.accept = 1 - cntr/(iter+cntr);

mround = round(mmean);

nnlist = nnlistall(:,1:mround);
tmp = rho.^(0:mround-1);
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

% create  Sx based on posterior mean of neigh
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


results.meth  = 'mess_g1';
results.bdraw = bsave;
results.adraw = asave;
results.bmean = bmean';
results.bstd  = bstd';
results.amean = amean;
results.astd  = astd;
results.smean = smean;
results.sdraw = ssave;
results.lmean = lmean;
results.mmean = mmean;
results.mstd  = mstd;
results.mdraw = msave;
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
results.palpha = palpha;
results.acov = S;
results.y = y;
results.yhat = yhat;
results.resid = e;
results.rsqr = rsqr;
results.rbar = rbar;
results.rho = rho;
results.q     = q;
results.ntime = gtime;
results.nobs = n;
results.nvar = k;
results.xflag = xflag;
results.nflag = nflag;


otherwise
   
end; % end of switch

function cout = c_mess1(alpha,ys,xs,Symat,beta,sige,neigh,mmat,a,B);
% PURPOSE: evaluate the conditional distribution of alpha 
%          for the Bayesian mess_g1 model
% ---------------------------------------------------
%  USAGE: cout = c_mess1(alpha,y,x,Symat,beta,alpha,neigh,mmat)
%  where:  alpha  = matrix exponential alpha spatial parameter
%            y    = dependent variable vector
%            x    = explanatory variables matrix
%          Symat  = matrix from mess_g1 
%                   containing a mesh of Sy for neigh values
%            beta = kx1 current bhat vector
%            sige = 1x1 current sige value
%           neigh = current neigh value (for lookup)
%            mmat = matrix of neigh values in the mesh
%               a = prior mean for alpha     (optional input)
%               B = prior variance for alpha (optional input)
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by mess_g1
%  --------------------------------------------------
%  SEE ALSO: mess_g1, mess_g1d
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


n = length(ys);
gsize = mmat(2,1) - mmat(1,1);
i1 = find(mmat <= neigh + gsize);
i2 = find(mmat <= neigh - gsize);
i1 = max(i1);
i2 = max(i2);
indexm = round((i1+i2)/2);
if isempty(indexm)
indexm = 1;
end;

Ycap = squeeze(Symat(:,:,indexm));
[junk nq] = size(Ycap);
nq1 = nq-1;
v = ones(nq,1);
for i=2:nq;
v(i,1) = alpha.^(i-1);
end;
W = (1./[1 cumprod(1:nq1)]);
Sy = Ycap*diag(W)*v;

e = Sy - xs*beta;
epe = e'*e;

if nargin == 8
cout =  -epe/(2*sige);
elseif nargin == 10
B = B*sige;
cout = -epe/(2*sige) - 0.5*(((alpha-a)^2)/B);
else
error('c_mess1: Wrong # of inputs arguments');
end;

   

function cout = m_mess1(neigh,ys,xs,Symat,beta,alpha,mmat);
% PURPOSE: evaluate the conditional distribution of alpha 
%          for the Bayesian mess_g1 model
% ---------------------------------------------------
%  USAGE: cout = m_mess1(neigh,y,x,Symat,beta,alpha,sige,mmat)
%  where:  neigh  = neighbors
%            y    = dependent variable vector
%            x    = explanatory variables matrix
%          Symat  = matrix from mess_g1 
%                   containing a mesh of Sy for neigh values
%            beta = kx1 current bhat vector
%           alpha = current alpha value 
%            mmat = matrix of neigh values in the mesh
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by mess_g1
%  --------------------------------------------------
%  SEE ALSO: mess_g1, mess_g1d
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


n = length(ys);
gsize = mmat(2,1) - mmat(1,1);
i1 = find(mmat <= neigh + gsize);
i2 = find(mmat <= neigh - gsize);
i1 = max(i1);
i2 = max(i2);
indexm = round((i1+i2)/2);
if isempty(indexm)
indexm = 1;
end;

Ycap = squeeze(Symat(:,:,indexm));

[junk nq] = size(Ycap);
nq1 = nq-1;
v = ones(nq,1);
for i=2:nq;
v(i,1) = alpha.^(i-1);
end;
W = (1./[1 cumprod(1:nq1)]);
Sy = Ycap*diag(W)*v;

e = Sy - xs*beta;
epe = e'*e;

cout = -(n/2)*log(epe); 




   