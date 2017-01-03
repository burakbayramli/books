function results = sarp_g(y,x,W,ndraw,nomit,prior)
% PURPOSE: Bayesian estimates of the spatial autoregressive probit model
%          y = rho*W*y + XB + e, e = N(0,I_n)
%          y is a binary 0,1 nx1 vector
%          B = N(c,T), 
%          1/sige = Gamma(nu,d0), 
%          rho = Uniform(rmin,rmax), or rho = beta(a1,a2); 
%-------------------------------------------------------------
% USAGE: results = sarp_g(y,x,W,ndraw,nomit,prior)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar), 
%            the intercept term (if present) must be in the first column of the matrix x
%        W = spatial weight matrix (standardized, row-sums = 1)
%    ndraw = # of draws
%    nomit = # of initial draws omitted for burn-in            
%    prior = a structure variable with:
%            prior.nsteps = # of samples used by truncated normal Gibbs sampler
%            prior.beta  = prior means for beta,   c above (default 0)
%            priov.bcov  = prior beta covariance , T above (default 1e+12)
%            prior.a1    = parameter for beta(a1,a2) prior on rho see: 'help beta_prior'
%            prior.a2    = (default = 1.0, a uniform prior on rmin,rmax) 
%            prior.eig   = 0 for default rmin = -1,rmax = +1, 1 for eigenvalue calculation of these
%            prior.rmin  = (optional) min rho used in sampling (default = -1)
%            prior.rmax  = (optional) max rho used in sampling (default = 1)  
%            prior.lflag = 0 for full lndet computation (default = 1, fastest)
%                        = 1 for MC approx (fast for large problems)
%                        = 2 for Spline approx (medium speed)
%            prior.order = order to use with prior.lflag = 1 option (default = 50)
%            prior.iter  = iters to use with prior.lflag = 1 option (default = 30) 
%            prior.lndet = a matrix returned by sar, sar_g, sarp_g, etc.
%                          containing log-determinant information to save time
%-------------------------------------------------------------
% RETURNS:  a structure:
%          results.meth     = 'sarp_g'
%          results.beta     = posterior mean of bhat based on draws
%          results.rho      = posterior mean of rho based on draws
%          results.sige     = posterior mean of sige based on draws
%          results.sigma    = posterior mean of sige based on (e'*e)/(n-k)
%          results.bdraw    = bhat draws (ndraw-nomit x nvar)
%          results.pdraw    = rho  draws (ndraw-nomit x 1)
%          results.sdraw    = sige draws (ndraw-nomit x 1)
%          results.total    = a matrix (ndraw,nvars-1) total x-impacts
%          results.direct   = a matrix (ndraw,nvars-1) direct x-impacts
%          results.indirect = a matrix (ndraw,nvars-1) indirect x-impacts
%          results.total_obs= a matrix (ndraw,nvars-1) observation-level total x-impacts
%          results.vmean  = mean of vi draws (nobs x 1) 
%          results.rdraw  = r draws (ndraw-nomit x 1) (if m,k input)
%          results.bmean  = b prior means, prior.beta from input
%          results.bstd   = b prior std deviations sqrt(diag(prior.bcov))
%          results.novi   = 1 for prior.novi = 1, 0 for prior.rval input
%          results.nobs   = # of observations
%          results.nvar   = # of variables in x-matrix
%          results.ndraw  = # of draws
%          results.nomit  = # of initial draws omitted
%          results.nsteps = # of samples used by Gibbs sampler for TMVN
%          results.y      = y-vector from input (nobs x 1)
%          results.zip    = # of zero y-values
%          results.yhat   = mean of posterior predicted (nobs x 1)
%          results.resid  = residuals, based on posterior means
%          results.rsqr   = r-squared based on posterior means
%          results.rbar   = adjusted r-squared
%          results.a1     = a1 parameter for beta prior on rho from input, or default value
%          results.a2     = a2 parameter for beta prior on rho from input, or default value
%          results.time1  = time for eigenvalue calculation
%          results.time2  = time for log determinant calcluation
%          results.time3  = time for sampling
%          results.time   = total time taken  
%          results.rmax   = 1/max eigenvalue of W (or rmax if input)
%          results.rmin   = 1/min eigenvalue of W (or rmin if input)          
%          results.tflag  = 'plevel' (default) for printing p-levels
%                         = 'tstat' for printing bogus t-statistics 
%          results.lflag  = lflag from input
%          results.cflag  = 1 for intercept term, 0 for no intercept term
%          results.iter   = prior.iter option from input
%          results.order  = prior.order option from input
%          results.limit  = matrix of [rho lower95,logdet approx, upper95] 
%                           intervals for the case of lflag = 1
%           results.lndet = a matrix containing log-determinant information
%                           (for use in later function calls to save time)
%           results.mlike = log marginal likelihood (a vector ranging over
%                           rho values that can be integrated for model comparison)
% --------------------------------------------------------------
% NOTES: - the intercept term (if you have one)
%          must be in the first column of the matrix x
% --------------------------------------------------------------
% SEE ALSO: (sarp_gd, sarp_gd2 demos) prt
% --------------------------------------------------------------
% REFERENCES: LeSage and Pace (2009) Chapter 10 on Bayesian estimation 
%             of spatial probit regression models.
% For lndet information see: Chapter 4 
%----------------------------------------------------------------

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com


timet = clock;

% error checking on inputs
[n junk] = size(y);
yin = y;

[n1, k] = size(x);
[n2, n4] = size(W);
time1 = 0;
time2 = 0;
time3 = 0;

nobsa = n;

results.nobs  = n;
results.nvar  = k;
results.y = y; 
results.zip = n - sum(y); % # of zero values in the y-vector

if nargin == 5
    prior.lflag = 1;
end;
  
[rho,rmin,rmax,detval,ldetflag,nsample,eflag,order,iter,novi_flag,c,T,inform_flag,a1,a2] = sarp_parse(prior,k);
    
% check if the user handled the intercept term okay
    n = length(y);
    if sum(x(:,1)) ~= n
    tst = sum(x); % we may have no intercept term
    ind = find(tst == n); % we do have an intercept term
     if length(ind) > 0
     error('sarp_g: intercept term must be in first column of the x-matrix');
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
    
    if n1 ~= n2
    error('sarp_g: wrong size weight matrix W');
    elseif n1 ~= n
    error('sarp_g: wrong size weight matrix W');
    end;
    [nchk junk] = size(y);
    if nchk ~= n
    error('sarp_g: wrong size y vector input');
    end;
    

results.order = order;
results.iter = iter;

timet = clock; % start the timer

[rmin,rmax,time1] = sar_eigs(eflag,W,rmin,rmax,n);
results.time1 = time1;

[detval,time2] = sar_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
results.time2 = time2;

% pre-calculate traces for the x-impacts calculations

iiter=50;
o=100;

diag_ests=zeros(n,o);
for iii=1:iiter

u=randn(n,1);

umat=u(:,ones(1,o));
wumat=zeros(n,o);
wu=u;
wumat(:,1)=wu;
for ii=2:o
    wu=W*wu;
    wumat(:,ii)=wu;
end
   
diag_estimates_iii=(umat.*wumat);

diag_ests=diag_ests+diag_estimates_iii;
end

estimated_diags=diag_ests/iiter;



% storage for draws
          bsave = zeros(ndraw-nomit,k);
          psave = zeros(ndraw-nomit,1);
          ymean = zeros(n,1);
          acc_rate = zeros(ndraw,1);

        total = zeros(ndraw-nomit,p);
        total_obs = zeros(n,p);
        direct = zeros(ndraw-nomit,p);
        indirect = zeros(ndraw-nomit,p);
        
        avg_total = zeros(p,1);
        avg_direct = zeros(p,1);
        avg_indirect = zeros(p,1);

% ====== initializations
% compute this stuff once to save time
TI = inv(T);
TIc = TI*c;
iter = 1;

in = ones(n,1);

Wy = sparse(W)*y;
Wadd = W + W';
WtW = W'*W;
sige = 1;


   
hwait = waitbar(0,'sarp: MCMC sampling ...');


t0 = clock;                  
iter = 1;
xpx = x'*x;
xpy = x'*y;
Wy = W*y;
xpWy = x'*Wy;

          ind1 = find(yin == 0);
          nobs0 = length(ind1);
          ind2 = find(yin == 1);
          nobs1 = length(ind2);
          if (nobs0 + nobs1 ~= n)
           error('sarp_g: not all y-values are 0 or 1');
          end;
        

          while (iter <= ndraw); % start sampling;
                  
          % update beta   
          AI = inv(xpx + sige*TI);        
          ys = y - rho*Wy;          
          b = x'*ys + sige*TIc;
          b0 = AI*b;
          bhat = norm_rnd(sige*AI) + b0;  
          xb = x*bhat;
           
 
      % we use numerical integration to perform rho-draw
          b0 = (x'*x)\(x'*y);
          bd = (x'*x)\(x'*Wy);
          e0 = y - x*b0;
          ed = Wy - x*bd;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
          rho = draw_rho(detval,epe0,eped,epe0d,n,k,rho,a1,a2);


          % update z-values,  

% loop over i
          hh = (speye(n) - rho*sparse(W));
          mu = hh\xb;
      
          tauinv = speye(n) - rho*Wadd + rho*rho*WtW;

          % tauinv = h'*h;
          aa = diag(tauinv);
          h = ones(n,1)./sqrt(aa);
          c = matdiv(-tauinv,aa);          
          ctilde = c - diag(diag(c));

          if iter == 1
          z = zeros(n,1);
          end;

          for initer=1:nsample;
            for i=1:n
            aa = ctilde(i,:)*z;
                muuse = (-mu(i,1)-aa)/h(i,1);
                if yin(i,1) == 0
                    t1=normrt_rnd(0,1,muuse);
                elseif yin(i,1) == 1
                    t1=normlt_rnd(0,1,muuse);
                end
            z(i,1) = aa + h(i,1)*t1  ;
            end
          end

          y = mu + z;
          
          % reformulate Wy
          Wy = sparse(W)*y;
                   

    if iter > nomit % if we are past burn-in, save the draws
    bsave(iter-nomit,1:k) = bhat';
    psave(iter-nomit,1) = rho;
    ymean = ymean + y;
    
    % compute effects estimates
    
        rhovec=(rho.^(0:o-1))';
        if cflag == 1
        beff = bhat(2:end,1);
        elseif cflag == 0
        beff = bhat;
        end;

        s = hh\speye(n);
        pdfz = stdn_pdf(mu);

        for kk=1:p;
  
          avg_direct(kk,1) = pdfz'*(estimated_diags*rhovec*beff(kk,1)/n); %av direct effect

          dd = spdiags(pdfz, 0, n, n);
          avg_total(kk,1) = mean(sum(dd*s*beff(kk,1),2));
          
          total_obs(:,kk) = total_obs(:,kk) + sum(dd*s*beff(kk,1),2);

          avg_indirect(kk,1) = avg_total(kk,1) - avg_direct(kk,1);

        end;
        
    total(iter-nomit,:) = avg_total';       % an ndraw-nomit x p matrix
    direct(iter-nomit,:) = avg_direct';     % an ndraw-nomit x p matrix
    indirect(iter-nomit,:) = avg_indirect'; % an ndraw-nomit x p matrix         


    end; % end of if iter > nomit
                    
iter = iter + 1; 
waitbar(iter/ndraw);         
end; % end of sampling loop
close(hwait);

time3 = etime(clock,t0);
results.time3 = time3;

total_obs = total_obs/(ndraw-nomit);



% compute posterior means
beta = mean(bsave)';
rho = mean(psave);
ymean = ymean/(ndraw-nomit);
results.sige = 1;
sige = 1;

% compute log marginal likelihood based on ymean (just an approximation)
[nobs,nvar] = size(x);
          Wy = W*ymean;
          AI = inv(x'*x + sige*TI);
          b0 = AI*(x'*ymean + sige*TIc);
          bd = AI*(x'*Wy + sige*TIc);
          e0 = ymean - x*b0;
          ed = Wy - x*bd;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;
 logdetx = log(det(x'*x + sige*TI));
  if inform_flag == 0
   mlike = sar_marginal(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,logdetx,a1,a2);
  elseif inform_flag == 1
   mlike = sar_marginal2(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,logdetx,a1,a2,c,TI,x,y,sige,W);
  end;


time = etime(clock,timet);

results.meth  = 'sarp_g';
results.ymean = ymean;
results.total = total;
results.direct = direct;
results.indirect = indirect;
results.total_obs = total_obs;
results.beta = beta;
results.rho = rho;
results.bdraw = bsave;
results.pdraw = psave;
results.bmean = c;
results.bstd  = sqrt(diag(T));
results.ndraw = ndraw;
results.nomit = nomit;
results.nsteps = nsample;
results.time  = time;
results.a1 = a1;
results.a2 = a2;
results.tflag = 'plevel';
results.rmax = rmax; 
results.rmin = rmin;
results.lflag = ldetflag;
results.lndet = detval;
results.priorb = inform_flag;
results.mlike = mlike;

% =========================================================================
% support functions below
% =========================================================================

function rho = draw_rho(detval,epe0,eped,epe0d,n,k,rho,a1,a2)

nmk = (n-k)/2;
nrho = length(detval(:,1));
iota = ones(nrho,1);

z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
z = -nmk*log(z);
%C = gammaln(nmk)*iota -nmk*log(2*pi)*iota - 0.5*logdetx*iota;
den =  detval(:,2) + z;

bprior = beta_prior(detval(:,1),a1,a2);
den = den + log(bprior);
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

% To see how this works, uncomment the following lines
% plot(detval(:,1),den/1000,'-');
% line([detval(idraw,1) detval(idraw,1)],[0 den(idraw,1)/1000]);
% hold on;
% line([detval(idraw,1) 0],[den(idraw,1)/1000 den(idraw,1)/1000]);
% drawnow;
% pause;



function [rho,rmin,rmax,detval,ldetflag,nsample,eflag,order,iter,novi_flag,c,T,inform_flag,a1,a2] = sarp_parse(prior,k)
% PURPOSE: parses input arguments for sar_g models
% ---------------------------------------------------
%  USAGE: [nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval, ...
%         ldetflag,eflag,mflag,order,iter,novi_flag,c,T,inform_flag,a1,a2,logmflag = 
%                           sar_parse(prior,k)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% set defaults

eflag = 0;     % default to not computing eigenvalues
ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
order = 50;    % there are parameters used by the MC det approx
iter = 30;     % defaults based on Pace and Barry recommendation
rmin = -1;     % use -1,1 rho interval as default
rmax = 1;
detval = 0;    % just a flag
rho = 0.5;
a1 = 1.0;
a2 = 1.0;
c = zeros(k,1);   % diffuse prior for beta
T = eye(k)*1e+12;
prior_beta = 0;   % flag for diffuse prior on beta
novi_flag = 0; % do vi-estimates
inform_flag = 0;
metflag = 0;
nsample = 1;

fields = fieldnames(prior);
nf = length(fields);
if nf > 0
 for i=1:nf
    if strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;   
    elseif strcmp(fields{i},'mhflag')
       metflag = prior.mhflag; 
    elseif strcmp(fields{i},'nsteps')
       nsample = prior.nsteps; 
    elseif strcmp(fields{i},'a1')
       a1 = prior.a1; 
    elseif strcmp(fields{i},'a2')
       a2 = prior.a2; 
    elseif strcmp(fields{i},'p')
       p = prior.p; 
    elseif strcmp(fields{i},'beta')
        c = prior.beta; inform_flag = 1; % flag for informative prior on beta
    elseif strcmp(fields{i},'bcov')
        T = prior.bcov; inform_flag = 1;
    elseif strcmp(fields{i},'rmin')
        rmin = prior.rmin; eflag = 0;
    elseif strcmp(fields{i},'rmax')
        rmax = prior.rmax;  eflag = 0;
    elseif strcmp(fields{i},'lndet')
    detval = prior.lndet;
    ldetflag = -1;
    eflag = 0;
    rmin = detval(1,1);
    nr = length(detval);
    rmax = detval(nr,1);
    elseif strcmp(fields{i},'lflag')
        tst = prior.lflag;
        if tst == 0,
        ldetflag = 0; 
        elseif tst == 1,
        ldetflag = 1; 
        elseif tst == 2,
        ldetflag = 2; 
        else
        error('sarp_g: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = prior.order;  
    elseif strcmp(fields{i},'iter')
        iter = prior.iter; 
    elseif strcmp(fields{i},'novi')
        novi_flag = prior.novi;
    elseif strcmp(fields{i},'dflag')
        metflag = prior.dflag;
    elseif strcmp(fields{i},'eig')
        eflag = prior.eig;
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


if eflag == 1 % compute eigenvalues
t0 = clock;
opt.tol = 1e-3; opt.disp = 0;
lambda = eigs(sparse(W),speye(n),1,'SR',opt);  
rmin = 1/real(lambda);   
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
            error('sarp_g: wrong lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('sarp_g: wrong sized lndet input argument');
        elseif n1 == 1
            error('sarp_g: wrong sized lndet input argument');
        end;          
end;


%Procedure for drawing from truncated multivariate normal based on
%Geweke's code. i.e draws from
%     xdraw is N(amu,sigma) subject to a < d*x < b
%     where N(.,) in the n-variate Normal, a and b are nx1
%Note that d is restricted to being a nonsingular nxn matrix
%la and lb are nx1 vectors set to one if no upper/lower bounds
%kstep=order of Gibbs within the constraint rows

function xdraw = tnorm_rnd(n,amu,sigma,a,b,la,lb,d,kstep);
niter=10;
%transform to work in terms of z=d*x
z=zeros(n,1);
dinv=inv(d);
anu=d*amu;

tau=d*sigma*d';
tauinv=inv(tau);
a1=a-anu;
b1=b-anu;
c=zeros(n,n);
h=zeros(n,1);
for i=1:n
aa=tauinv(i,i);
h(i,1)=1/sqrt(aa);
for j=1:n
c(i,j)=-tauinv(i,j)/aa;
end
end

for initer=1:niter
for i1=1:n
i=kstep(i1,1);
aa=0;
for j=1:n
if (i ~= j);
aa=aa+c(i,j)*z(j,1);
end
end

if la(i,1)==1
    t1=normrt_rnd(0,1,(b1(i,1)-aa)/h(i,1));
elseif lb(i,1)==1
    t1=normlt_rnd(0,1,(a1(i,1)-aa)/h(i,1));
else
t1=normt_rnd(0,1,(a1(i,1)-aa)/h(i,1),(b1(i,1)-aa)/h(i,1));
end
z(i,1)=aa+h(i,1)*t1  ;
end
end

%Transform back to x
xdraw=dinv*z;
for i=1:n
xdraw(i,1)=xdraw(i,1)+amu(i,1);
end


function result = normrt_rnd(mu,sigma2,right)
% PURPOSE: compute random draws from a right-truncated normal
%          distribution, with mean = mu, variance = sigma2
% ------------------------------------------------------
% USAGE: y = normrt_rnd(mu,sigma2,right)
% where: nobs = # of draws
%          mu = mean     (scalar or vector)
%      sigma2 = variance (scalar or vector)
%       right = right truncation point (scalar or vector)
% ------------------------------------------------------
% RETURNS: y = (scalar or vector) the size of mu, sigma2
% ------------------------------------------------------
% NOTES: This is merely a convenience function that
%        calls normt_rnd with the appropriate arguments
% ------------------------------------------------------

% written by:
% James P. LeSage, Dept of Finance & Economics
% Texas State Univeristy-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com
% last updated 10/2007

if nargin ~= 3
error('normrt_rnd: Wrong # of input arguments');
end;

nobs = length(mu);
left = -999*ones(nobs,1);

result = normt_rnd(mu,sigma2,left,right);

 
function result = normlt_rnd(mu,sigma2,left)
% PURPOSE: compute random draws from a left-truncated normal
%          distribution, with mean = mu, variance = sigma2
% ------------------------------------------------------
% USAGE: y = normlt_rnd(mu,sigma2,left)
% where:   mu = mean (scalar or vector)
%      sigma2 = variance (scalar or vector)
%        left = left truncation point (scalar or vector)
% ------------------------------------------------------
% RETURNS: y = (scalar or vector) the size of mu, sigma2
% ------------------------------------------------------
% NOTES: This is merely a convenience function that
%        calls normt_rnd with the appropriate arguments
% ------------------------------------------------------

% written by:
% James P. LeSage, Dept of Finance & Economics
% Texas State Univeristy-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com
% Last updated 10/2007

if nargin ~= 3
error('normlt_rnd: Wrong # of input arguments');
end;

nobs = length(mu);
right = 999*ones(nobs,1);

result = normt_rnd(mu,sigma2,left,right);

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
% NOTES: -this does not take any prior on beta, sigma into account, uses diffuse priors
%         see sar_marginal2() 
% -------------------------------------------------------------------------

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com



n = length(detval);
nmk = (nobs-nvar)/2;
% C is a constant of integration that can vary with nvars, so for model
% comparisions involving different nvars we need to include this
bprior = beta_prior(detval(:,1),a1,a2);
C = log(bprior) + gammaln(nmk) - nmk*log(2*pi) - 0.5*logdetx;
iota = ones(n,1);
z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
den = C + detval(:,2) - nmk*log(z);
out = real(den);

function  out = sar_marginal2(detval,e0,ed,epe0,eped,epe0d,nobs,nvar,a1,a2,c,TI,xs,ys,sige,W)
% PURPOSE: returns a vector of the log-marginal over a grid of rho-values
%          for the case of an informative prior on beta
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
%                 c = prior mean for beta
%                TI = prior var-cov for beta
%                xs = x*sqrt(V) or x if homoscedastic model
%                ys = y*sqrt(V) or y is homoscedastic model
% -------------------------------------------------------------------------
% RETURNS: out = a structure variable
%        out = log marginal, a vector the length of detval
% -------------------------------------------------------------------------
% NOTES: -this is only an approximation based on the posterior mean Vi-estimates
% -------------------------------------------------------------------------

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com

n = length(detval);
nmk = (nobs-nvar)/2;
% C is a constant of integration that can vary with nvars, so for model
% comparisions involving different nvars we need to include this
bprior = beta_prior(detval(:,1),a1,a2);
C = log(bprior) + gammaln(nmk) - nmk*log(2*pi);
iota = ones(n,1);
z = epe0*iota - 2*detval(:,1)*epe0d + detval(:,1).*detval(:,1)*eped;
% add quadratic terms based on prior for beta
Q1 = zeros(n,1);
Q2 = zeros(n,1);
xpxi = inv(xs'*xs);
sTI = sige*TI;
xpxis = inv(xs'*xs + sTI);
logdetx = log(det(xpxis));
C = C - 0.5*logdetx;
          for i=1:n;
           rho = detval(i,1);
           D = speye(nobs) - rho*W;
           bhat = xpxi*(xs'*D*ys);
           beta = xpxis*(xs'*D*ys + sTI*c); 
           Q1(i,1) = (c - beta)'*sTI*(c - beta);
           Q2(i,1) = (bhat - beta)'*(xs'*xs)*(bhat - beta);
          end;

den = C + detval(:,2) - nmk*log(z + Q1 + Q2);
out = real(den);

