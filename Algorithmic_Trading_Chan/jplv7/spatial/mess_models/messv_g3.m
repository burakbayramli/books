function results = messv_g3(y,x,options,ndraw,nomit,prior,start)
% PURPOSE: Bayesian estimates of the matrix exponential spatial model (mess)
% % [samples values of rho and #neighbors to produce a posterior distribution
%    for these hyperparameters in the problem]
% S*y = X*b + e,           with xflag == 0, or:
% S*y = [i X D*X]*b + e,   with xflag == 1
%          e = N(0,sige*V), V = diag(v1,...,vn)
%          S = e^alpha*D
%          r/vi = ID chi(r)/r, r = Gamma(m,k)
%          B = N(c,T), 
%          1/sige = Gamma(nu,d0), 
%          alpha = N(a,B), (default: diffuse prior) 
% D = a weight matrix constructed from neighbors using:
% D = sum rho^i N_i / sum rho^i, i=1,...,#neighbors
% using a grid of rho values from options.rmin to options.rmax
% and a grid of values options.nmin to options.nmax over the # of neighbors, i
% where i = # neighbors, N_i a matrix with ith nearest neighbors
%-------------------------------------------------------------
% USAGE: results = mess_g3(y,x,options,ndraw,nomit,prior,start)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%  options = a structure variable with:
%  options.latt  = lattitude coordinates (nx1 vector)
%  options.long  = longitude coordinates (nx1 vector)
%  options.mmin = minimum # of neighbors to search over (default = 4)
%  options.mmax = maximum # of neighbors to search over (default = 10)
%  options.rmin  = minimum value of rho to use (0 < rho < 1), (default 0.5)                  
%  options.rmax  = maximum value of rho to use in discounting (default 1)
%  options.xflag = 0 for S*y = X*b + e,         model (default)
%                = 1 for S*y = [i X D*X]*b + e, model
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
%            prior.rval  = r prior hyperparameter, default=4
%            prior.m,    = informative Gamma(m,k) prior on r
%            prior.k,    = informative Gamma(m,k) prior on r
%            prior.lflag = 0 (default for marginal likelihood calculations)
%                        = 1 for no marginal likelihood (faster)
%    start = (optional) structure containing starting values: 
%            defaults: beta=ones(k,1),sige=1,rho=0.5, V=ones(n,1)
%            start.b   = beta starting values (nvar x 1)
%            start.a   = alpha starting value (scalar)
%            start.sig = sige starting value  (scalar)
%-------------------------------------------------------------
% RETURNS:  a structure:
%          results.meth   = 'messv_g3'
%          results.bdraw  = bhat draws (ndraw-nomit x nvar)
%          results.bmean  = mean of bhat draws
%          results.bstd   = std of bhat draws
%          results.adraw  = alpha draws (ndraw-nomit x 1)
%          results.amean  = mean of alpha draws
%          results.astd   = std of alpha draws
%          results.sdraw  = sige draws (ndraw-nomit x 1)
%          results.smean  = mean of sige draws
%          results.lmean  = marginal likelihood based on mean of draws
%          results.rmean  = posterior mean of rho
%          results.rstd   = posterior std of rho
%          results.rdraw  = draws for rho
%          results.mmean  = posterior mean of m, the # of neighbors
%          results.mstd   = posterior std of  m, the # of neighbors
%          results.mdraw  = draws for # of neighbors
%          results.vmean  = posterior mean of vi draws
%          results.bprior = b prior means, prior.beta from input
%          results.bpstd  = b prior std deviations sqrt(diag(prior.bcov))
%          results.nobs   = # of observations
%          results.nvar   = # of variables in x-matrix (plus D*X matrix)
%          results.ndraw  = # of draws
%          results.nomit  = # of initial draws omitted
%          results.y      = y-vector from input (nobs x 1)
%          results.r      = value of hyperparameter r (if input)
%          results.rvdraw = r draws (ndraw-nomit x 1) (if m,k input)
%          results.yhat   = mean of posterior predicted (nobs x 1)
%          results.nu     = nu prior parameter
%          results.d0     = d0 prior parameter
%          results.stime  = time for sampling
%          results.time   = total time taken  
%          results.ntime  = time taken for setup mesh over rho,m values
%          results.accept = acceptance rate for alpha <= 0
%          results.rmax   = rmax: default (or user input)
%          results.rmin   = rmin: default (or user input)   
%          results.mmax   = mmax: default (or user input)
%          results.mmin   = mmin: default (or user input)    
%          results.nflag  = nflag value from input (or default)   
%          results.tflag  = 'plevel' (default) for printing p-levels
%                         = 'tstat' for printing bogus t-statistics 
%          results.palpha   = prior for alpha (from input)
%          results.acov   = prior variance for alpha (from input)
%          results.pflag  = 1, if a normal(a,B) prior for alpha, 0 otherwise
%          results.xflag = model flag from input
%          results.q     = q value from input (or default)

% --------------------------------------------------------------
% NOTES: if the model includes a constant term
% it should be entered as the first column in the x-matrix
% that is input to the function
% 1) mess_g1 produces a posterior distribution for # neighbors
% 2) mess_g2 produces a posterior distribution for the hyperparameter rho
% 3) mess_g3 produces posteriors for both rho and # of neighbors
% --------------------------------------------------------------
% SEE ALSO:  mess_g3d, mess_g, mess_g3, messv_g, prt, c_mess, mess
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
error('messv_g3: x-matrix contains wrong # of observations');
end;

% set defaults
q = 7;
xflag = 0;
rmin = 0.5;
rmax = 1.0;
rval = 4;
mmin = 4;
mmax = 10;
neigh = 0;
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
        error('messv_g3: must supply starting values in a structure');
    end;
 % parse starting values entered by the user
 fields = fieldnames(start);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'b')
        b0 = start.b; 
        [n1 n2] = size(b0); % error checking on user inputs
       if n1 ~= k
        error('messv_g3: starting beta values are wrong');
       elseif n2 ~= 1
        error('messv_g3: starting beta values are wrong');
       end;
    elseif strcmp(fields{i},'sig')
        sig0 = start.sig;
       [n1 n2] = size(sig0); % error checking on user inputs
       if n1 ~= 1
        error('messv_g3: starting sige value is wrong');
       elseif n2 ~= 1
        error('messv_g3: starting sige value is wrong');
       end;
    elseif strcmp(fields{i},'a')
        astart = start.a;
       [n1 n2] = size(astart); % error checking on user inputs
       if n1 ~= 1
        error('messv_g3: starting alpha value is wrong');
       elseif n2 ~= 1
        error('messv_g3: starting alpha value is wrong');
       end;
    end;
 end; % end of for loop
% parse options structure
    if ~isstruct(options)
        error('messv_g3: must supply option values in a structure');
    end;
 fields = fieldnames(options);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'xflag')
       xflag = options.xflag;
    elseif strcmp(fields{i},'rmin')
        rmin = options.rmin;
    elseif strcmp(fields{i},'rmax')
        rmax = options.rmax;
    elseif strcmp(fields{i},'mmin')
        mmin = options.mmin;
    elseif strcmp(fields{i},'mmax')
        mmax = options.mmax;
    elseif strcmp(fields{i},'nflag')
        nflag = options.nflag;
    elseif strcmp(fields{i},'q')
       q = options.q;
     elseif strcmp(fields{i},'latt')
        latt = options.latt; llflag = llflag + 1;
     elseif strcmp(fields{i},'long')
        long = options.long; llflag = llflag + 1;
     end;
 end; % end of for loop

% parse prior structure variable inputs        
    if ~isstruct(prior)
    error('messv_g3: must supply the prior as a structure variable');
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
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval
    elseif strcmp(fields{i},'rval');
        rval = prior.rval;
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
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval
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
    elseif strcmp(fields{i},'nflag')
        nflag = options.nflag;
    end;
 end;
 
 % parse options
     if ~isstruct(options)
        error('messv_g3: must supply option values in a structure');
    end;
 fields = fieldnames(options);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'xflag')
       xflag = options.xflag;
    elseif strcmp(fields{i},'rmin')
        rmin = options.rmin;
    elseif strcmp(fields{i},'rmax')
        rmax = options.rmax;
    elseif strcmp(fields{i},'mmin')
        mmin = options.mmin;
    elseif strcmp(fields{i},'mmax')
        mmax = options.mmax;
    elseif strcmp(fields{i},'q')
       q = options.q;
    elseif strcmp(fields{i},'nflag')
        nflag = options.nflag;
     elseif strcmp(fields{i},'latt')
        latt = options.latt; llflag = llflag + 1;
     elseif strcmp(fields{i},'long')
        long = options.long; llflag = llflag + 1;
     end;
 end; % end of for loop



elseif nargin == 5   % we supply all defaults
   % parse options structure
    if ~isstruct(options)
        error('messv_g3: must supply option values in a structure');
    end;
 fields = fieldnames(options);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'xflag')
       xflag = options.xflag;
    elseif strcmp(fields{i},'rmin')
        rmin = options.rmin;
    elseif strcmp(fields{i},'rmax')
        rmax = options.rmax;
    elseif strcmp(fields{i},'mmin')
        mmin = options.mmin;
    elseif strcmp(fields{i},'mmax')
        mmax = options.mmax;
    elseif strcmp(fields{i},'q')
       q = options.q;
     elseif strcmp(fields{i},'latt')
        latt = options.latt; llflag = llflag + 1;
     elseif strcmp(fields{i},'long')
        long = options.long; llflag = llflag + 1;
    elseif strcmp(fields{i},'nflag')
        nflag = options.nflag;
     end;
 end; % end of for loop

else
error('Wrong # of arguments to messv_g3');
end;
      

% error checking on prior information inputs
[checkk,junk] = size(c);
if checkk ~= k
error('messv_g3: prior means are wrong');
elseif junk ~= 1
error('messv_g3: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('messv_g3: prior bcov is wrong');
elseif junk ~= k
error('messv_g3: prior bcov is wrong');
end;

[checkk junk] = size(palpha);
if checkk ~= 1
error('messv_g3: prior alpha is wrong');
elseif junk ~= 1
error('messv_g3: prior alpha is wrong');
end;

[checkk junk] = size(S);
if checkk ~= 1
error('messv_g3: prior acov is wrong');
elseif junk ~= 1
error('messv_g: prior acov is wrong');
end;

% make sure the user input latt, long or we really bomb
if llflag ~= 2;
error('messv_g3: no lattitude-longitude coordinates input');
end;

switch xflag % switch on x transformation

   
   case{0} % case where x variables are not transformed


% ====== initializations
% compute this stuff once to save time
TI = inv(T);
TIc = TI*c;

% ========= do up front grid over rho, alpha values
t1 = clock;   % time this operation

results.rmin = rmin;
results.rmax = rmax;
results.mmin = mmin;
results.mmax = mmax;

rgrid = rmin:0.01:rmax;
mgrid = mmin:1:mmax;
nrho = length(rgrid);
nneigh = length(mgrid);


% storage for Y over the grid
Ymat = zeros(n,q,nrho,nneigh); % vectors of Sy for various alpha,rho values

% find index into nearest neighbors
if nflag == 0
nnlistall = find_nn(latt,long,mmax);
elseif nflag == 1
nnlistall = find_nn(latt,long,mmax,3);
else
error('mess_g3: bad nflag option');
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

% do grid over rho, neigh values
hwait = waitbar(0,'computing grid over rho and neighbors ...');
ngrid = nneigh*nrho;
iter = 1;

for kk=1:nneigh;
neigh = mgrid(kk);
nnlist = nnlistall(:,1:neigh);
for jj=1:nrho;
rho = rgrid(jj);

tmp = rho.^(0:neigh-1);
tmp = tmp/sum(tmp);

% construct and save Y
wy = y;
Y = y(:,ones(1,q));
for i=2:q;
wy = wy(nnlist)*tmp';
Y(:,i) = wy;
end;
% we can save Y for lookup
Ymat(:,:,jj,kk) = Y;
% save rho
rmat(jj,1) = rho;

end; % end of loop over alpha values 
mmat(kk,1) = neigh;

iter = iter + nrho; 
waitbar(iter/ngrid);         

end; % end of loop over neighbors
close(hwait);

% end of up front stuff with Sy saved in Symat
gtime = etime(clock,t1);

% initializations and starting values for the sampler
rho = 1;
alpha = astart;
neigh = mmax;
cc=0.2;   % initial metropolis value
cnta = 0; % counter for acceptance rate for alpha
iter = 1;
in = ones(n,1);
sige = sig0;
V = ones(n,1);

% storage for draws
          bsave = zeros(ndraw-nomit,k);
          asave = zeros(ndraw-nomit,1);
          ssave = zeros(ndraw-nomit,1);
          rsave = zeros(ndraw-nomit,1);
          msave = zeros(ndraw-nomit,1);
          if mm ~= 0
          rdraw = zeros(ndraw-nomit,1);
          else
          rdraw = 0;
          end;
          vmean = zeros(n,1);
	       lsave = 0;
          rtmp = zeros(nomit,1);

hwait = waitbar(0,'MCMC sampling ...');
t0 = clock;                  
iter = 1;
          while (iter <= ndraw); % start sampling;


          
          % lookup Ymat based on alpha, rho, neigh values
          gsize = rmat(2,1) - rmat(1,1);
          i1 = find(rmat <= rho + gsize);
          i2 = find(rmat <= rho - gsize);
          i1 = max(i1);
          i2 = max(i2);
          indexr = round((i1+i2)/2);
          if isempty(indexr)
          indexr = 1;
          end;
          gsize = mmat(2,1) - mmat(1,1);
          i1 = find(mmat <= neigh + gsize);
          i2 = find(mmat <= neigh - gsize);
          i1 = max(i1);
          i2 = max(i2);
          indexm = round((i1+i2)/2);
          if isempty(indexm)
          indexm = 1;
          end;

          Ycap = squeeze(Ymat(:,:,indexr,indexm));
          Ycaps = matmul(Ycap,sqrt(V));
          % create Sy based on Y
          [junk nq] = size(Ycap);
          nq1 = nq-1;
          v = ones(nq,1);
          for i=2:nq;
          v(i,1) = alpha.^(i-1);
          end;
          W = (1./[1 cumprod(1:nq1)]);
          Sy = Ycap*diag(W)*v;
          Sys = Ycaps*diag(W)*v;
          xs = matmul(x,sqrt(V)); 

          % update beta   
          AI = inv(xs'*xs + sige*TI);
          b = x'*Sys + sige*TIc;
          b0 = AI*b;
          bhat = norm_rnd(sige*AI) + b0;  
         
          % update sige
          nu1 = n + 2*nu; 
          e = (Sys - xs*bhat);
          d1 = 2*d0 + e'*e;
          chi = chis_rnd(1,nu1);
          sige = d1/chi;

          % update vi
          e = Sy - x*bhat;
          chiv = chis_rnd(n,rval+1);   
          vi = ((e.*e./sige) + in*rval)./chiv;
          V = in./vi; 
  
          % update rval
          if mm ~= 0           
          rval = gamm_rnd(1,1,mm,kk);  
          end;
                       
          % metropolis step to get alpha update
          if pflag == 0
          alphax = cn_mess3(alpha,y,x,Ymat,bhat,sige,rho,neigh,rmat,mmat); 
          elseif pflag == 1
          alphax = cn_mess3(alpha,y,x,Ymat,bhat,sige,rho,neigh,rmat,mmat,palpha,S); 
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
           alphay = cn_mess3(alpha2,y,x,Ymat,bhat,sige,rho,neigh,rmat,mmat);
          elseif pflag == 1
           alphay = cn_mess3(alpha2,y,x,Ymat,bhat,sige,rho,neigh,rmat,mmat,palpha,S);
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

          % update rho using metroplis-hastings step
          rhox = rn_mess3(rho,y,x,Ymat,bhat,alpha,neigh,rmat,mmat); 
          rho2 = unif_rnd(1,rmin,rmax);

          rhoy = rn_mess3(rho2,y,x,Ymat,bhat,alpha,neigh,rmat,mmat);
           ru = unif_rnd(1,0,1);
          if ((rhoy - rhox) > exp(1)),
          p = 1;
          else,          
          ratio = exp(rhoy-rhox);
          p = min(1,ratio);
          end;
              if (ru < p)
                 rho = rho2;
              end;

          % update neigh using metroplis-hastings step
          neighx = nn_mess3(neigh,y,x,Ymat,bhat,alpha,rho,rmat,mmat); 
          neigh2 = round(unif_rnd(1,mmin,mmax));

          neighy = nn_mess3(neigh2,y,x,Ymat,bhat,alpha,rho,rmat,mmat);
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
    rsave(iter-nomit,1) = rho;
    msave(iter-nomit,1) = neigh;
    vmean = vmean + vi;
    if mm~= 0
    rdraw(iter-nomit) = rval;
    end;

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
vmean = vmean/(iter-nomit);
amean = mean(asave);
bmean = mean(bsave);
astd = std(asave);
bstd = std(bsave);
smean = mean(ssave);
rmean = mean(rsave);
rstd = std(rsave);
mmean = mean(msave);
mstd = std(msave);

% find acceptance rate
results.accept = 1 - cnta/(iter+cnta);
% NOTE: this could be interpreted as the
% probability that alpha is in the mesh grid

% compute Sy based on posterior means for rho, neigh, alpha
mround = round(mmean);
tmp = rmean.^(0:mround-1);
tmp = tmp/sum(tmp);

% find index into nearest neighbors
nnlist = nnlistall(:,1:mround);

wy = y;
Y = y(:,ones(1,q));
for i=2:q;
wy = wy(nnlist)*tmp';
Y(:,i) = wy;
end;
Ys = matmul(Y,sqrt(vmean));

[junk nq] = size(Y);
nq1 = nq-1;
v = ones(nq,1);
for i=2:nq;
v(i,1) = amean.^(i-1);
end;
W = (1./[1 cumprod(1:nq1)]);
sy = Y*diag(W)*v;
sys = Ys*diag(W)*v;

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

results.meth  = 'messv_g3';
results.bdraw = bsave;
results.rdraw = rsave;
results.adraw = asave;
results.bmean = bmean';
results.bstd  = bstd';
results.amean = amean;
results.astd  = astd;
results.smean = smean;
results.sdraw = ssave;
results.rmean = rmean;
results.rstd  = rstd;
results.rdraw = rsave;
results.mmean = mmean;
results.mstd  = mstd;
results.mdraw = msave;
results.lmean = lmean;
results.vmean = vmean;
results.rvdraw = rdraw;
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
if mm~= 0
results.m = m;
results.k = k;
else
results.rval = rval;
end;
results.tflag = 'plevel';
results.pflag = pflag;
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

results.rmin = rmin;
results.rmax = rmax;
results.mmin = mmin;
results.mmax = mmax;

rgrid = rmin:0.01:rmax;
mgrid = mmin:1:mmax;
nrho = length(rgrid);
nneigh = length(mgrid);

t1 = clock;   % time this operation

% storage for Y,X over the grid
Ymat = zeros(n,q,nrho,nneigh); % vectors of Sy for various alpha,rho,neigh values
Xmat = zeros(n,2*k-1,nrho,nneigh); % matrices of Sx for various alpha,rho,neigh values
rmat = zeros(nrho,1);   % save rho values
mmat = zeros(nneigh,1); % save neigh values

% we have to construct the weight matrix using neighbors
if nflag == 0
nnlistall = find_nn(latt,long,mmax);
elseif nflag == 1
nnlistall = find_nn(latt,long,mmax,3);
else
error('messv_g3: bad nflag option');
end;

% check for empty nnlist columns
chk = find(nnlistall == 0);
if length(chk) > 0; 
 if nflag == 1 % no saving the user here
 error('messv_g3: trying too many neighbors, some do not exist');
 else % we save the user here
 nnlistall = find_nn(latt,long,mmax,4);
 end;
end;

% do grid over rho, neigh values
hwait = waitbar(0,'computing grid over rho and neighbors ...');
ngrid = nneigh*nrho;
iter = 1;

for kk=1:nneigh;
neigh = mgrid(kk);
nnlist = nnlistall(:,1:neigh);
for jj=1:nrho;
rho = rgrid(jj);

tmp = rho.^(0:neigh-1);
tmp = tmp/sum(tmp);

% construct and save Y
wy = y;
Y = y(:,ones(1,q));
for i=2:q;
wy = wy(nnlist)*tmp';
Y(:,i) = wy;
end;

Ymat(:,:,jj,kk) = Y;
% create and save X
[junk nk] = size(xsub);
xout = x;
for i=1:nk;
xi = xsub(:,i);
tmpp = xi(nnlist)*tmp';
xout = [xout tmpp];
end;
Xmat(:,:,jj,kk) = xout;
% save rho
rmat(jj,1) = rho;

end; % end of loop over rho values 
mmat(kk,1) = neigh;
iter = iter + nrho; 
waitbar(iter/ngrid);         

end; % end of loop over neighbors

close(hwait);

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
V = ones(n,1);

% storage for draws
          bsave = zeros(ndraw-nomit,kk);
          asave = zeros(ndraw-nomit,1);
          ssave = zeros(ndraw-nomit,1);
          rsave = zeros(ndraw-nomit,1);
          msave = zeros(ndraw-nomit,1);
          if mm ~= 0
          rdraw = zeros(ndraw-nomit,1);
          else
          rdraw = 0;
          end;

          vmean = zeros(n,1);
	       lsave = 0;
          rtmp = zeros(nomit,1);

hwait = waitbar(0,'MCMC sampling ...');
t0 = clock;                  
iter = 1;
          while (iter <= ndraw); % start sampling;

          % lookup Sx based on rho values
          gsize = rmat(2,1) - rmat(1,1);
          i1 = find(rmat <= rho + gsize);
          i2 = find(rmat <= rho - gsize);
          i1 = max(i1);
          i2 = max(i2);
          indexr = round((i1+i2)/2);
          if isempty(indexr)
          indexr = 1;
          end;
          gsize = mmat(2,1) - mmat(1,1);
          i1 = find(mmat <= neigh + gsize);
          i2 = find(mmat <= neigh - gsize);
          i1 = max(i1);
          i2 = max(i2);
          indexm = round((i1+i2)/2);
          if isempty(indexm)
          indexm = 1;
          end;

          Ycap = squeeze(Ymat(:,:,indexr,indexm));
          Ycaps = matmul(Ycap,sqrt(V));

          Xcap= squeeze(Xmat(:,:,indexr,indexm));
          Xcaps = matmul(Xcap,sqrt(V));

          % construct Sy based on look up values
          [junk nq] = size(Ycap);
          nq1 = nq-1;
          v = ones(nq,1);
          for i=2:nq;
          v(i,1) = alpha.^(i-1);
          end;
          W = (1./[1 cumprod(1:nq1)]);
          Sy = Ycap*diag(W)*v;
          Sys = Ycaps*diag(W)*v;

          % update beta   
          AI = inv(Xcaps'*Xcaps + sige*TI);
          b = Xcaps'*Sys + sige*TIc;
          b0 = AI*b;
          bhat = norm_rnd(sige*AI) + b0;  
         
          % update sige
          nu1 = n + 2*nu; 
          e = (Sys - Xcaps*bhat);
          d1 = 2*d0 + e'*e;
          chi = chis_rnd(1,nu1);
          sige = d1/chi;

          % update vi
          e = Sy - Xcap*bhat;
          chiv = chis_rnd(n,rval+1);   
          vi = ((e.*e./sige) + in*rval)./chiv;
          V = in./vi; 
  
          % update rval
          if mm ~= 0           
          rval = gamm_rnd(1,1,mm,kk);  
          end;


          % metropolis step to get alpha update
          if pflag == 0
          alphax = cn_mess3(alpha,y,Xcap,Ymat,bhat,sige,rho,neigh,rmat,mmat); 
          elseif pflag == 1
          alphax = cn_mess3(alpha,y,Xcap,Ymat,bhat,sige,rho,neigh,rmat,mmat,palpha,S); 
          end;
          accept = 0;
          alpha2 = alpha + cc*randn(1,1);
          while accept == 0
           if (alpha2 <= 0) 
           accept = 1;  
           else
           alpha2 = alpha + cc*randn(1,1);
           cntr = cntr+1; % counts accept rate
           end; 
          end; 
           if pflag == 0
           alphay = cn_mess3(alpha2,y,Xcap,Ymat,bhat,sige,rho,neigh,rmat,mmat);
           elseif pflag == 1
           alphay = cn_mess3(alpha2,y,Xcap,Ymat,bhat,sige,rho,neigh,rmat,mmat,palpha,S);
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

          % update rho using metroplis-hastings step
          rhox = rn_mess3(rho,y,Xcap,Ymat,bhat,alpha,neigh,rmat,mmat); 
          rho2 = unif_rnd(1,rmin,rmax);
          rhoy = rn_mess3(rho2,y,Xcap,Ymat,bhat,alpha,neigh,rmat,mmat);
           ru = unif_rnd(1,0,1);
          if ((rhoy - rhox) > exp(1)),
          p = 1;
          else,          
          ratio = exp(rhoy-rhox);
          p = min(1,ratio);
          end;
              if (ru < p)
                 rho = rho2;
              end;
	  
          % update neigh using metroplis-hastings step
          neighx = nn_mess3(neigh,y,Xcap,Ymat,bhat,alpha,rho,rmat,mmat); 
          neigh2 = round(unif_rnd(1,mmin,mmax));

          neighy = nn_mess3(neigh2,y,Xcap,Ymat,bhat,alpha,rho,rmat,mmat);
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
    rsave(iter-nomit,1) = rho;
    msave(iter-nomit,1) = neigh;
    if mm~= 0
    rdraw(iter-nomit,1) = rval;
    end;
    vmean = vmean + vi;

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
rmean = mean(rsave);
rstd = std(rsave);
mmean = mean(msave);
mstd = std(msave);
vmean = vmean/(ndraw-nomit);

% find acceptance rate
results.accept = 1 - cntr/(iter+cntr);

mround = round(mmean);

tmp = rmean.^(0:mround-1);
tmp = tmp/sum(tmp);

nnlist = nnlistall(:,1:mround);

% construct Sy, Sx based on posterior means
wy = y;
Y = y(:,ones(1,q));
for i=2:q;
wy = wy(nnlist)*tmp';
Y(:,i) = wy;
end;
Ys = matmul(Y,sqrt(vmean));
[junk nq] = size(Y);
nq1 = nq-1;
v = ones(nq,1);
for i=2:nq;
v(i,1) = amean.^(i-1);
end;
W = (1./[1 cumprod(1:nq1)]);
sy = Y*diag(W)*v;
sys = Ys*diag(W)*v;

% create  Sx based on posterior mean of rho
xmat = x;
for i=1:nk;
xi = xsub(:,i);
tmpp = xi(nnlist)*tmp';
xmat = [xmat tmpp];
end;
xmats = matmul(xmat,sqrt(vmean));

e = sy - xmat*bmean';
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

results.meth  = 'messv_g3';
results.bdraw = bsave;
results.adraw = asave;
results.bmean = bmean';
results.bstd  = bstd';
results.amean = amean;
results.astd  = astd;
results.smean = smean;
results.sdraw = ssave;
results.lmean = lmean;
results.rmean = rmean;
results.rstd  = rstd;
results.rdraw = rsave;
results.mmean = mmean;
results.mstd  = mstd;
results.mdraw = msave;
results.vmean = vmean;
results.rvdraw = rdraw;
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
if mm~= 0
results.m = m;
results.k = k;
else
results.rval = rval;
end;
results.tflag = 'plevel';
results.pflag = pflag;
results.palpha = palpha;
results.acov = S;
results.y = y;
results.yhat = yhat;
results.resid = e;
results.rsqr = rsqr;
results.rbar = rbar;
results.q     = q;
results.ntime = gtime;
results.nobs = n;
results.nvar = k;
results.xflag = xflag;
results.nflag = nflag;

otherwise
   
end; % end of switch


function cout = rn_mess3(rho,ys,xs,Ymat,beta,alpha,neigh,rmat,mmat);
% PURPOSE: evaluate the conditional distribution of alpha 
%          for the Bayesian mess_g3 model
% ---------------------------------------------------
%  USAGE: cout = c_mess3(alpha,y,x,Symat,beta,alpha,rho,neigh,amat,rmat,mmat)
%  where:  alpha  = matrix exponential alpha spatial parameter
%            y    = dependent variable vector
%            x    = explanatory variables matrix
%          Symat  = matrix from mess_g3 
%                   containing a mesh of Sy for alph,rho,m values
%            beta = kx1 current bhat vector
%            rho  = current rho value (for lookup)
%           neigh = current m value (for lookup)
%            amat = matrix of alpha values in the mesh
%            rmat = matrix of rho values in the mesh
%            mmat = matrix of neigh values in the mesh
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by mess_g3
%  --------------------------------------------------
%  SEE ALSO: mess_g3, mess_g3d
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


n = length(ys);
% lookup Sy based on alpha, rho values
gsize = rmat(2,1) - rmat(1,1);
i1 = find(rmat <= rho + gsize);
i2 = find(rmat <= rho - gsize);
i1 = max(i1);
i2 = max(i2);
indexr = round((i1+i2)/2);
if isempty(indexr)
indexr = 1;
end;
gsize = mmat(2,1) - mmat(1,1);
i1 = find(mmat <= neigh + gsize);
i2 = find(mmat <= neigh - gsize);
i1 = max(i1);
i2 = max(i2);
indexm = round((i1+i2)/2);
if isempty(indexm)
indexm = 1;
end;

Ycap = squeeze(Ymat(:,:,indexr,indexm));

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


function cout = cn_mess3(alpha,ys,xs,Ymat,beta,sige,rho,neigh,rmat,mmat,a,B);
% PURPOSE: evaluate the conditional distribution of alpha 
%          for the Bayesian mess_g3 model
% ---------------------------------------------------
%  USAGE: cout = c_mess3(alpha,y,x,Symat,beta,sige,rho,neigh,rmat,mmat,a,B)
%  where:  alpha  = matrix exponential alpha spatial parameter
%            y    = dependent variable vector
%            x    = explanatory variables matrix
%          Symat  = matrix from mess_g3 
%                   containing a mesh of Sy for rho,m values
%            beta = kx1 current bhat vector
%            sige = 1x1 current sige value
%            rho  = current rho value (for lookup)
%           neigh = current m value (for lookup)
%            rmat = matrix of rho values in the mesh
%            mmat = matrix of neigh values in the mesh
%               a = prior mean for alpha     (optional input)
%               B = prior variance for alpha (optional input)
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by mess_g3
%  --------------------------------------------------
%  SEE ALSO: mess_g3, mess_g3d
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


n = length(ys);
% lookup Sy based on alpha, rho values
gsize = rmat(2,1) - rmat(1,1);
i1 = find(rmat <= rho + gsize);
i2 = find(rmat <= rho - gsize);
i1 = max(i1);
i2 = max(i2);
indexr = round((i1+i2)/2);
if isempty(indexr)
indexr = 1;
end;
gsize = mmat(2,1) - mmat(1,1);
i1 = find(mmat <= neigh + gsize);
i2 = find(mmat <= neigh - gsize);
i1 = max(i1);
i2 = max(i2);
indexm = round((i1+i2)/2);
if isempty(indexm)
indexm = 1;
end;

Ycap = squeeze(Ymat(:,:,indexr,indexm));

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

if nargin == 10
cout =  -epe/(2*sige);
elseif nargin == 12
B = B*sige;
cout = -epe/(2*sige) - 0.5*(((alpha-a)^2)/B);
else
error('cn_mess3: Wrong # of inputs arguments');
end;



function cout = nn_mess3(neigh,ys,xs,Ymat,beta,alpha,rho,rmat,mmat);
% PURPOSE: evaluate the conditional distribution of alpha 
%          for the Bayesian mess_g2 model
% ---------------------------------------------------
%  USAGE: cout = c_mess3(alpha,y,x,Symat,beta,alpha,rho,neigh,amat,rmat,mmat)
%  where:  alpha  = matrix exponential alpha spatial parameter
%            y    = dependent variable vector
%            x    = explanatory variables matrix
%          Symat  = matrix from mess_g3 
%                   containing a mesh of Sy for rho,m values
%            beta = kx1 current bhat vector
%            rho  = current rho value (for lookup)
%           neigh = current m value (for lookup)
%            rmat = matrix of rho values in the mesh
%            mmat = matrix of neigh values in the mesh
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by mess_g3
%  --------------------------------------------------
%  SEE ALSO: mess_g3, mess_g3d
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


n = length(ys);
% lookup Sy based on alpha, rho values
gsize = rmat(2,1) - rmat(1,1);
i1 = find(rmat <= rho + gsize);
i2 = find(rmat <= rho - gsize);
i1 = max(i1);
i2 = max(i2);
indexr = round((i1+i2)/2);
if isempty(indexr)
indexr = 1;
end;
gsize = mmat(2,1) - mmat(1,1);
i1 = find(mmat <= neigh + gsize);
i2 = find(mmat <= neigh - gsize);
i1 = max(i1);
i2 = max(i2);
indexm = round((i1+i2)/2);
if isempty(indexm)
indexm = 1;
end;

Ycap = squeeze(Ymat(:,:,indexr,indexm));

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

