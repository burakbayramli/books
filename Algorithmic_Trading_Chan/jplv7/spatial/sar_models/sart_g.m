function results = sart_g(y,x,W,ndraw,nomit,prior)
% PURPOSE: Bayesian estimates of the spatial autoregressive tobit model
%          y = rho*W*y + XB + e, e = N(0,sige*Omega), Omega = inv[(I_n-rho*W)'*(I_n -rho*W)]
%          y is a binary 0,1 nx1 vector
%          B = N(c,T), 
%          1/sige = Gamma(nu,d0), 
%          rho = beta(a1,a2) prior 
%-------------------------------------------------------------
% USAGE: results = sart_g(y,x,W,ndraw,nomit,prior)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar), 
%            the intercept term (if present) must be in the first column of the matrix x
%        W = spatial weight matrix (standardized, row-sums = 1)
%    ndraw = # of draws
%    nomit = # of initial draws omitted for burn-in            
%    prior = a structure variable with:
%            prior.beta   = prior means for beta,   c above (default 0)
%            priov.bcov   = prior beta covariance , T above (default 1e+12)
%            prior.mhflag = 1 for M-H sampling of rho (default = 0)
%            prior.nsteps = # of Gibbs steps for Geweke z procedure
%            prior.eig    = 0 for computing eigenvalues of W-matrix
%                          (defaults to 1, uses rmin = -1, rmax = 1)
%            prior.a1     = parameter for beta(a1,a2) prior on rho see: 'help beta_prior'
%            prior.a2     = (default = 1.0, a uniform prior on rmin,rmax) 
%            prior.rmin   = (optional) min rho used in sampling (default = -1)
%            prior.rmax   = (optional) max rho used in sampling (default = 1)  
%            prior.lflag  = 0 for full lndet computation (default = 1, fastest)
%                         = 1 for MC approx (fast for large problems)
%                         = 2 for Spline approx (medium speed)
%            prior.order  = order to use with prior.lflag = 1 option (default = 50)
%            prior.iter   = iters to use with prior.lflag = 1 option (default = 30) 
%            prior.lndet  = a matrix returned by sar, sar_g, sarp_g, etc.
%                           containing log-determinant information to save time
%-------------------------------------------------------------
% RETURNS:  a structure:
%          results.meth   = 'sart_g'
%          results.bdraw  = bhat draws (ndraw-nomit x nvar)
%          results.pdraw  = rho  draws (ndraw-nomit x 1)
%          results.total    = a 3-d matrix (ndraw,nvars-1,ntrs) total x-impacts
%          results.direct   = a 3-d matrix (ndraw,nvars-1,ntrs) direct x-impacts
%          results.indirect = a 3-d matrix (ndraw,nvars-1,ntrs) indirect x-impacts
%                             ntrs defaults to 101 trace terms
%          results.nsteps   = nsteps from input
%          results.nobs   = # of observations
%          results.nvar   = # of variables in x-matrix
%          results.ndraw  = # of draws
%          results.nomit  = # of initial draws omitted
%          results.y      = y-vector from input (nobs x 1)
%          results.yhat   = mean of predicted based on posterior parameter means
%                           i.e., yhat = inv(I_n - rho*W)*x*beta (nobs x 1)
%          results.yprob  = stdn_cdf(yhat)
%          results.zip    = # of zero y-values
%          results.time1  = time for eigenvalue calculation
%          results.time2  = time for log determinant calcluation
%          results.time3  = time for sampling
%          results.time   = total time taken  
%          results.rmax   = 1/max eigenvalue of W (or rmax if input)
%          results.rmin   = 1/min eigenvalue of W (or rmin if input)          
%          results.tflag  = 'plevel' (default) for printing p-levels
%                         = 'tstat' for printing bogus t-statistics 
%          results.lflag  = lflag from input
%          results.iter   = prior.iter option from input
%          results.order  = prior.order option from input
%          results.wlimit  = matrix of [rho lower95,logdet approx, upper95] 
%                           intervals for the case of lflag = 1
%          results.lndet = a matrix containing log-determinant information
%                          (for use in later function calls to save time)
%          results.priorb= a flag for diffuse or informative prior on b
%          results.acc    = an ndraw x 1 vector of acceptance rates for M-H sampling
%          results.cflag  = 1 if there is an intercept, 0 otherwise
% --------------------------------------------------------------
% REFERENCES: LeSage and Pace (2009) 
% Introduction to Spatial Econometrics, Chapter 10.
%----------------------------------------------------------------

% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com


timet = clock;

% error checking on inputs
[n junk] = size(y);
[n1 k] = size(x);
[n3 n4] = size(W);
yin = y;

time1 = 0;
time2 = 0;
time3 = 0;

if n1 ~= n
error('sart_g: x-matrix contains wrong # of observations');
elseif n3 ~= n4
error('sart_g: W matrix is not square');
elseif n3~= n
error('sart_g: W matrix is not the same size at y,x');
end;

% check if the user handled the intercept term okay
    n = length(y);
    if sum(x(:,1)) ~= n
    tst = sum(x); % we may have no intercept term
    ind = find(tst == n); % we do have an intercept term
     if length(ind) > 0
     error('sart_g: intercept term must be in first column of the x-matrix');
     elseif length(ind) == 0 % case of no intercept term
     cflag = 0;
     p = size(x,2);
     end;
    elseif sum(x(:,1)) == n % we have an intercept in the right place
     cflag = 1;
     p = size(x,2)-1;
    end;
     
    results.cflag = cflag;
    results.p = p;
    
    

if nargin == 5
    prior.lflag = 1;
end;

[rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,c,T,prior_beta,cc,metflag,vflag,nu,d0,a1,a2,nsample] = sart_parse(prior,k);


          ind = find(y <= vflag);
          
nobsc = length(ind);


% error checking on prior information inputs
[checkk,junk] = size(c);
if checkk ~= k
error('sart_g: prior means are wrong');
elseif junk ~= 1
error('sart_g: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('sart_g: prior bcov is wrong');
elseif junk ~= k
error('sart_g: prior bcov is wrong');
end;

results.y = y;      
results.nobs = n;
results.nvar = k;   
results.order = order;
results.iter = iter;

timet = clock; % start the timer

[rmin,rmax,time2] = sar_eigs(eflag,W,rmin,rmax,n);

[detval,time1] = sar_lndet(ldetflag,W,rmin,rmax,detval,order,iter);

% storage for draws
          bsave = zeros(ndraw-nomit,k);
          psave = zeros(ndraw-nomit,1);
          ssave = zeros(ndraw-nomit,1);
          ymean = zeros(n,1);
          acc_rate = zeros(ndraw,1);


% ====== initializations
% compute this stuff once to save time
TI = inv(T);
TIc = TI*c;
acc = 0;

Wy = sparse(W)*y;

hwait = waitbar(0,'sart\_g: MCMC sampling ...');
t0 = clock;                  
iter = 1;
xpx = (x'*x);

          ind1 = find(yin == 0);
          nobs0 = length(ind1);
          ind2 = find(yin > 0);
          nobs1 = length(ind2);
          
          while (iter <= ndraw); % start sampling;
                  
          % update beta   
          AI = inv(xpx + sige*TI);        
          ys = y - rho*Wy;          
          b = x'*ys + sige*TIc;
          b0 = AI*b;
          bhat = norm_rnd(sige*AI) + b0;  
          xb = x*bhat;
          
          % update sige
          nu1 = n + 2*nu; 
          e = (ys - xb);
          d1 = 2*d0 + e'*e;
          chi = chis_rnd(1,nu1);
          sige = d1/chi;
          
         if metflag == 1
         % metropolis step to get rho update
          rhox = c_sar(rho,y,xb,sige,W,detval);
          accept = 0; 
          rho2 = rho + cc*randn(1,1); 
          while accept == 0
           if ((rho2 > rmin) & (rho2 < rmax)); 
           accept = 1;  
           else
           rho2 = rho + cc*randn(1,1);
           end; 
          end;
          rhoy = c_sar(rho2,y,xb,sige,W,detval);
          ru = unif_rnd(1,0,1);
          if ((rhoy - rhox) > exp(1)),
          pp = 1;
          else, 
          ratio = exp(rhoy-rhox); 
          pp = min(1,ratio);
          end;
              if (ru < pp)
              rho = rho2;
              acc = acc + 1;
              end;
      acc_rate(iter,1) = acc/iter;
      % update cc based on std of rho draws
       if acc_rate(iter,1) < 0.4
       cc = cc/1.1;
       end;
       if acc_rate(iter,1) > 0.6
       cc = cc*1.1;
       end;
    end;

         if metflag == 0
      % when metflag == 0,
      % we use numerical integration to perform rho-draw
          b0 = (x'*x)\(x'*y);
          bd = (x'*x)\(x'*Wy);
          e0 = y - x*b0;
          ed = Wy - x*bd;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
          rho = draw_rho(detval,epe0,eped,epe0d,n,k,rho);
      end;


          % update z-values, 
if iter == 1
    z = zeros(n,1);
end;

z = zeros(n,1);

% loop over i
          h = (speye(n) - rho*sparse(W));
          mu = h\xb;
          
          tauinv = (h'*h)/sige;
          aa = diag(tauinv);
          h = ones(n,1)./sqrt(aa);
          c = matdiv(-tauinv,aa);          
          ctilde = c - diag(diag(c));

          for initer=1:nsample;
            for i=1:n
                if yin(i,1) == 0
                aa = ctilde(i,:)*z;
                muuse = (-mu(i,1)-aa)/h(i,1);
                t1=normrt_rnd(0,1,muuse);
                z(i,1) = aa + h(i,1)*t1;
                end;
            end
          end

          y(ind1,1) = mu(ind1,1) + z(ind1,1);
          
          % reformulate Wy
          Wy = sparse(W)*y;
          
          
                  
         
    if iter > nomit
    bsave(iter-nomit,1:k) = bhat';
    psave(iter-nomit,1) = rho;
    ssave(iter-nomit,1) = sige;
    ymean = ymean + y;
    end;

              
iter = iter + 1;

waitbar(iter/ndraw);         
end; % end of sampling loop
close(hwait);

% calculate effects estimates
        
t0 = clock; 

% pre-calculate traces for the x-impacts calculations
uiter=50;
maxorderu=100;
nobs = n;
rv=randn(nobs,uiter);
tracew=zeros(maxorderu,1);
wjjju=rv;
for jjj=1:maxorderu
    wjjju=W*wjjju;
    tracew(jjj)=mean(mean(rv.*wjjju));
    
end

traces=[tracew];
traces(1)=0;
traces(2)=sum(sum(W'.*W))/nobs;
trs=[1;traces];
ntrs=length(trs);
trbig=trs';
                 
        if cflag == 1
        bdraws = bsave(:,2:end);
        elseif cflag == 0
        bdraws = bsave;
        end; 
        pdraws = psave;

        ree = 0:1:ntrs-1;

        rmat = zeros(1,ntrs);
        total = zeros(ndraw-nomit,p,ntrs);
        direct = zeros(ndraw-nomit,p,ntrs);
        indirect = zeros(ndraw-nomit,p,ntrs);
        
for i=1:ndraw-nomit;
    rmat = pdraws(i,1).^ree;
    for j=1:p;
            beta = [bdraws(i,j)];
            total(i,j,:) = beta(1,1)*rmat;
    direct(i,j,:) = (beta*trbig).*rmat;
    indirect(i,j,:) = total(i,j,:) - direct(i,j,:);
    end;

end;

time4 = etime(clock,t0);
results.time4 = time4;


ymean = ymean/(ndraw-nomit);
bsave = bsave(nomit+1:end,:);
psave = psave(nomit+1:end,1);
ssave = ssave(nomit+1:end,1);

bmean = mean(bsave);
beta = bmean';
rho = mean(psave);
sige = mean(ssave);

yhat = (speye(n) - rho*W)\(x*beta);

time3 = etime(clock,t0);

time = etime(clock,timet);

results.meth  = 'sart_g';
results.beta = beta;
results.rho = rho;
results.bdraw = bsave;
results.pdraw = psave;
results.sdraw = ssave;
results.yhat  = yhat;
results.ymean = ymean;
results.nsteps = nsample;
results.total = total;
results.direct = direct;
results.indirect = indirect;
results.bmean = c;
results.bstd  = sqrt(diag(T));
results.nobs  = n;
results.nvar  = k;
results.ndraw = ndraw;
results.nomit = nomit;
results.time  = time;
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.tflag = 'plevel';
results.acc = acc_rate;
results.order = order;
results.rmax = rmax; 
results.rmin = rmin;
results.lflag = ldetflag;
results.lndet = detval;
results.priorb = prior_beta;
results.limit = vflag;
results.trunc = cflag;
results.nobsc = nobsc;


% ==================
% support functions
% ==================


function rho = draw_rho(detval,epe0,eped,epe0d,n,k,rho)
% update rho via univariate numerical integration

nmk = (n-k)/2;
nrho = length(detval(:,1));
iota = ones(nrho,1);

z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
den = detval(:,2) - nmk*log(z);
n = length(den);
y = detval(:,1);
adj = max(den);
den = den - adj;
x = exp(den);

% trapezoid rule
isum = sum((y(2:n,1) + y(1:n-1,1)).*(x(2:n,1) - x(1:n-1,1))/2);
z = abs(x/isum);
den = cumsum(z);

rnd = unif_rnd(1,0,1)*sum(z);
ind = find(den <= rnd);
idraw = max(ind);
if (idraw > 0 & idraw < nrho)
rho = detval(idraw,1);
end;



function cout = c_sar(rho,y,xb,sige,W,detval,c,T);
% PURPOSE: evaluate the conditional distribution of rho given sige
%  spatial autoregressive model using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:cout = c_sar(rho,y,x,b,sige,W,detval,p,R)
%  where:  rho  = spatial autoregressive parameter
%          y    = dependent variable vector
%          W    = spatial weight matrix
%        detval = an (ngrid,2) matrix of values for det(I-rho*W) 
%                 over a grid of rho values 
%                 detval(:,1) = determinant values
%                 detval(:,2) = associated rho values
%          sige = sige value
%          p    = (optional) prior mean for rho
%          R    = (optional) prior variance for rho
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by sar_g
%  --------------------------------------------------
%  SEE ALSO: sar_g, c_far, c_sac, c_sem
% ---------------------------------------------------

gsize = detval(2,1) - detval(1,1);
% Note these are actually log detvalues
i1 = find(detval(:,1) <= rho + gsize);
i2 = find(detval(:,1) <= rho - gsize);
i1 = max(i1);
i2 = max(i2);
index = round((i1+i2)/2);
if isempty(index)
index = 1;
end;
detm = detval(index,2); 

if nargin == 6      % case of diffuse prior
n = length(y);
z = speye(n) - rho*sparse(W);
e = z*y - xb; 
epe = (e'*e)/(2*sige);

elseif nargin == 8  % case of informative prior
z = speye(n) - rho*sparse(W);
e = z*y - xb; 
n = length(y);
T = T*sige;
z = (speye(n) - rho*W)*e;
epe = ((z'*z)/2*sige) + 0.5*(((rho-c)^2)/T);

else
error('c_sar: Wrong # of inputs arguments');

end;

cout =   detm - epe;

function [rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,c,T,prior_beta,cc,metflag,vflag,nu,d0,a1,a2,nsample] = sart_parse(prior,k)
% PURPOSE: parses input arguments for sart_g models
% ---------------------------------------------------
%  USAGE: [nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,novi_flag,c,T,prior_beta,cc,metflag] = 
%                           sar_parse(prior,k)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% set defaults
cflag = 0;   % default to left censoring 
vflag = 0.0; % at zero
nu = 0;
d0 = 0;
nsample = 5;

nsample = 1;  % default Gibbs steps for Geweke z-star sampling
eflag = 1;     % default to not computing eigenvalues
ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
order = 50;    % there are parameters used by the MC det approx
iter = 30;     % defaults based on Pace and Barry recommendation
rmin = -0.99;     % use -1,1 rho interval as default
rmax = 0.99;
detval = 0;    % just a flag
rho = 0.5;
sige = 1.0;
c = zeros(k,1);   % diffuse prior for beta
T = eye(k)*1e+12;
prior_beta = 0;   % flag for diffuse prior on beta
cc=0.1;
metflag = 0;
a1 = 1.0;
a2 = 1.0;


fields = fieldnames(prior);
nf = length(fields);
if nf > 0
 for i=1:nf
    if strcmp(fields{i},'beta')
        c = prior.beta;
        prior_beta = 1; % flag for informative prior on beta
    elseif strcmp(fields{i},'trunc');
      if strcmp(prior.trunc,'left');
      cflag = 0;
      else
      cflag = 1;
      end;
    elseif strcmp(fields{i},'limit');
        vflag = prior.limit;       
    elseif strcmp(fields{i},'nsteps')
       nsample = prior.nsteps; 
    elseif strcmp(fields{i},'a1')
       a1 = prior.a1; 
    elseif strcmp(fields{i},'a2')
       a2 = prior.a2; 
    elseif strcmp(fields{i},'bcov')
        T = prior.bcov;
        prior_beta = 1; % flag for informative prior on beta
    elseif strcmp(fields{i},'rmin')
        rmin = prior.rmin;  eflag = 1;
    elseif strcmp(fields{i},'rmax')
        rmax = prior.rmax;  eflag = 1;
    elseif strcmp(fields{i},'lndet')
    detval = prior.lndet;
    ldetflag = -1;
    eflag = 1;
    rmin = detval(1,1);
    nr = length(detval);
    rmax = detval(nr,1);
    elseif strcmp(fields{i},'lflag')
        tst = prior.lflag;
        if tst == 0,
        ldetflag = 0; eflag = 0; % compute eigenvalues
        elseif tst == 1,
        ldetflag = 1; eflag = 1; % reset this from default
        elseif tst == 2,
        ldetflag = 2; eflag = 1; % reset this from default
        else
        error('sart_g: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = prior.order;  
    elseif strcmp(fields{i},'iter')
        iter = prior.iter; 
    elseif strcmp(fields{i},'mhflag')
        metflag = prior.mhflag;
    end;
 end;
 
else, % the user has input a blank info structure
      % so we use the defaults
end; 

function [rmin,rmax,time2] = sar_eigs(eflag,W,rmin,rmax,n);
% PURPOSE: compute the eigenvalues for the weight matrix
% ---------------------------------------------------
%  USAGE: [rmin,rmax,time2] = far_eigs(eflag,W,rmin,rmax,W)
% where eflag is an input flag, W is the weight matrix
%       rmin,rmax may be used as default outputs
% and the outputs are either user-inputs or default values
% ---------------------------------------------------


if eflag == 0
t0 = clock;
opt.tol = 1e-3; opt.disp = 0;
lambda = eigs(sparse(W),speye(n),1,'SR',opt);  
rmin = 1/lambda;   
rmax = 1;
time2 = etime(clock,t0);
else
time2 = 0;
end;


function [detval,time1] = sar_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
% PURPOSE: compute the log determinant |I_n - rho*W|
% using the user-selected (or default) method
% ---------------------------------------------------
%  USAGE: detval = far_lndet(lflag,W,rmin,rmax)
% where eflag,rmin,rmax,W contains input flags 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------


% do lndet approximation calculations if needed
if ldetflag == 0 % no approximation
t0 = clock;    
out = lndetfull(W,rmin,rmax);
time1 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];
    
elseif ldetflag == 1 % use Pace and Barry, 1999 MC approximation

t0 = clock;    
out = lndetmc(order,iter,W,rmin,rmax);
time1 = etime(clock,t0);
results.limit = [out.rho out.lo95 out.lndet out.up95];
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];

elseif ldetflag == 2 % use Pace and Barry, 1998 spline interpolation

t0 = clock;
out = lndetint(W,rmin,rmax);
time1 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval = [tt' outi];

elseif ldetflag == -1 % the user fed down a detval matrix
    time1 = 0;
        % check to see if this is right
        if detval == 0
            error('sar_g: wrgon lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('sar_g: wrong sized lndet input argument');
        elseif n1 == 1
            error('sar_g: wrong sized lndet input argument');
        end;          
end;

function  out = sar_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,logdetx,a1,a2)
% PURPOSE: returns a vector of the log-marginal over a grid of rho-values
% -------------------------------------------------------------------------
% USAGE: out = sar_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,logdetx,a1,a2)
% where:       detval = an ngrid x 2 matrix with rho-values and lndet values
%                  e0 = y - x*b0;
%                 ed = Wy - x*bd;
%               epe0 = e0'*e0;
%               eped = ed'*ed;
%              epe0d = ed'*e0;
%               nobs = # of observations
%               nvar = # of explanatory variables
%            logdetx = log(det(x'*x))
%                 a1 = parameter for beta prior on rho
%                 a2 = parameter for beta prior on rho
% -------------------------------------------------------------------------
% RETURNS: out = a structure variable
%        out = log marginal, a vector the length of detval
% -------------------------------------------------------------------------

% written by:
% James P. LeSage, 7/2003
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

n = length(detval);
nmk = (nobs-nvar);
C =  nvar*gammaln(0.5) + gammaln((nmk)/2) - gammaln(nobs/2);
% C is a constant of integration that can vary with nvars, so for model
% comparisions involving different nvars we need to include this
iota = ones(n,1);
z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
den = -0.5*logdetx*iota + detval(:,2) - (nmk/2)*log(z);
den = real(den);
bprior = beta_prior(detval(:,1),a1,a2);
den = den + log(bprior);
out = C*iota + den;



function [total,direct,indirect] = calc_sar_effects(maxorder1,trs,strs,ntrs,rho,bhat,p,cflag)
% function to compute effects for 1 rho, 1 vector beta

% eliminate the constant term if it is in the model
if cflag == 0
    beta = bhat;
elseif cflag == 1
    beta = bhat(2:end,1);
end;


%this forms the matrix of exponents needed to form G
saa=kron(ones(maxorder1,1),strs)';
saav=saa(:);
saavsub=saav(1:(ntrs*maxorder1));
ree1=reshape(saavsub,ntrs,maxorder1)';
op=ones(p,1);
trbig=trs(:,op)';

    arparmri=rho;
   
    %forming P
    blockbsdm=reshape(beta',p,maxorder1);
    
       
    %forming G
    ree=arparmri.^ree1;   
    reblock=ree(1:maxorder1,1:maxorder1);
    ree(1:maxorder1,1:maxorder1)=reblock-tril(reblock)+(diag(ones(maxorder1,1)));
    
    %forming PG
    pg=blockbsdm*ree;%pg is also the total impacts by order (p by ntrs)
   
    %added contribution for iteration i
    pgtrbig=pg.*trbig;%direct
    pginbig=pg-pgtrbig;%indirect
    
total = pg;
direct = pgtrbig;
indirect = pginbig;


function bounds = cr_interval(adraw,hperc)
% PURPOSE: Computes an hperc-percent credible interval for a vector of MCMC draws
% --------------------------------------------------------------------
% Usage: bounds = cr_interval(draws,hperc);
% where draws = an ndraw by nvar matrix
%       hperc = 0 to 1 value for hperc percentage point
% --------------------------------------------------------------------
% RETURNS:
%         bounds = a 1 x 2 vector with 
%         bounds(1,1) = 1-hperc percentage point
%         bounds(1,2) = hperc percentage point
%          e.g. if hperc = 0.95
%          bounds(1,1) = 0.05 point for 1st vector in the matrix
%          bounds(1,2) = 0.95 point  for 1st vector in the matrix
%          bounds(2,1) = 0.05 point for 2nd vector in the matrix
%          bounds(2,2) = 0.05 point for 2nd vector in the matrix
%          ...
% --------------------------------------------------------------------

% Written by J.P. LeSage

% This function takes a vector of MCMC draws and calculates
% an hperc-percent credible interval
[ndraw,ncols]=size(adraw);
botperc=round((0.50-hperc/2)*ndraw);
topperc=round((0.50+hperc/2)*ndraw);
bounds = zeros(ncols,2);
for i=1:ncols;
temp = sort(adraw(:,i),1);
bounds(i,:) =[temp(topperc,1) temp(botperc,1)];
end;
