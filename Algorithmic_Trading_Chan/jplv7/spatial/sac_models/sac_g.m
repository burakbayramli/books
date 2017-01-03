function results = sac_g(y,x,W1,W2,ndraw,nomit,prior)
% PURPOSE: Bayesian estimates of the general spatial model
%          y = rho*W1*y + XB + u, u = lambda*W2 + e
%          W1,W2 can be the same matrix or different
%          e = N(0,sige*V), V = diag(v1,v2,...vn) 
%          r/vi = ID chi(r)/r, r = 4 is the default
%          B = N(c,T), 
%          1/sige = Gamma(nu,d0), 
%          rho,lambda = Uniform(rmin,rmax), or rho,lambda = beta(a1,a2); 
%-------------------------------------------------------------
% USAGE: results = sac_g(y,x,W1,W2,ndraw,nomit,prior)
% where: y = dependent variable vector (nobs x 1)
%          x = explanatory variables matrix, (with intercept term in first
%              column if used)
%        W1,W2 = spatial weight matrices (standardized, row-sums = 1)
%    ndraw = # of draws
%    nomit = # of initial draws omitted for burn-in            
%    prior = a structure variable with:
%            prior.beta  = prior means for beta,   c above (default 0)
%            priov.bcov  = prior beta covariance , T above (default 1e+12)
%            prior.novi  = 1 turns off sampling for vi, producing homoscedastic model            
%            prior.rval  = r prior hyperparameter, default=4
%            prior.nu    = informative Gamma(nu,d0) prior on sige
%            prior.d0    = default: nu=0,d0=0 (diffuse prior)
%            prior.a1    = parameter for beta(a1,a2) prior on rho see: 'help beta_prior'
%            prior.a2    = (default = 1.0, a uniform prior on rmin,rmax) 
%            prior.rmin  = (optional) min rho used in sampling (default = -1)
%            prior.rmax  = (optional) max rho used in sampling (default = +1)  
%            prior.lmin  = (optional) min lambda used in sampling (default = -1)
%            prior.lmax  = (optional) max lambda used in sampling (default = +1)  
%            prior.eigs  = 0 to compute rmin/rmax using eigenvalues, (1 = don't compute default)
%            prior.lflag = 0 for full lndet computation (default = 1, fastest)
%                        = 1 for MC approx (fast for large problems)
%                        = 2 for Spline approx (medium speed)
%            prior.order = order to use with prior.lflag = 1 option (default = 50)
%            prior.iter  = iters to use with prior.lflag = 1 option (default = 30)   
%            prior.lndet1 = a matrix returned by sar, sem, sar_g, etc. containing 
%                          log-determinant information for the W1 matrix to save time
%            prior.lndet2 = a matrix returned by sar, sem, sar_g, etc. containing
%                          log-determinant information for the W2 matrix to save time
%-------------------------------------------------------------
% RETURNS:  a structure:
%          results.meth   = 'sac_g'
%          results.beta   = posterior mean of bhat
%          results.rho    = posterior mean of rho
%          results.lambda = posterior mean of lambda
%          results.sige   = posterior mean of sige
%          results.bdraw  = bhat draws (ndraw-nomit x nvar)
%          results.pdraw  = rho  draws (ndraw-nomit x 1)
%          results.ldraw  = lambda draws (ndraw-nomit x 1)
%          results.sdraw  = sige draws (ndraw-nomit x 1)
%          results.total    = a 3-d matrix (ndraw,nvars-1,ntrs) total x-impacts
%          results.direct   = a 3-d matrix (ndraw,nvars-1,ntrs) direct x-impacts
%          results.indirect = a 3-d matrix (ndraw,nvars-1,ntrs) indirect x-impacts
%                             ntrs defaults to 101 trace terms
%          results.vmean  = mean of vi draws (nobs x 1) 
%          results.bmean  = b prior means, prior.beta from input
%          results.bstd   = b prior std deviations sqrt(diag(prior.bcov))
%          results.r      = value of hyperparameter r (if input)
%          results.nobs   = # of observations
%          results.nvar   = # of variables in x-matrix
%          results.ndraw  = # of draws
%          results.nomit  = # of initial draws omitted
%          results.y      = y-vector from input (nobs x 1)
%          results.yhat   = mean of posterior predicted (nobs x 1)
%          results.resid  = residuals, based on posterior means
%          results.rsqr   = r-squared based on posterior means
%          results.rbar   = adjusted r-squared
%          results.nu     = nu prior parameter
%          results.d0     = d0 prior parameter
%          results.a1     = a1 parameter for beta prior on rho from input, or default value
%          results.a2     = a2 parameter for beta prior on rho from input, or default value
%          results.time1  = time for eigenvalue calculation
%          results.time2  = time for log determinant calcluation
%          results.time3  = time for sampling
%          results.time   = total time taken  
%          results.rmax   = 1/max eigenvalue of W1 (or rmax if input)
%          results.rmin   = 1/min eigenvalue of W1 (or rmin if input)   
%          results.lmax   = 1/max eigenvalue of W2 (or lmax if input)
%          results.lmin   = 1/min eigenvalue of W2 (or lmin if input)   
%          results.tflag  = 'plevel' (default) for printing p-levels
%                         = 'tstat' for printing bogus t-statistics 
%          results.lflag  = lflag from input
%          results.iter   = prior.iter option from input
%          results.order  = prior.order option from input
%          results.limit  = matrix of [rho lower95,logdet approx, upper95] 
%                           intervals for the case of lflag = 1
%          results.lndet1 = a matrix containing log-determinant information for the W1 matrix
%                          (for use in later function calls to save time)
%          results.lndet2 = a matrix containing log-determinant information for the W2 matrix
%                          (for use in later function calls to save time)
%          results.acc   = acceptance rate for M-H sampling (ndraw x 1) vector
%          results.cflag = 0 for no intercept term, 1 for intercept term
%          results.p     = # of non-constant explanatory variables
% --------------------------------------------------------------
% NOTES: - uses an improper prior for the rvalue 
%          or informative Gamma prior.m, prior.k, not both of them
% - for n < 500 you should use lflag = 0 to get exact results  
% - use a1 = 1.0 and a2 = 1.0 for uniform prior on rho
% --------------------------------------------------------------
% SEE ALSO: (sac_gd, sac_gd2 demos) prt
% --------------------------------------------------------------
% REFERENCES: James P. LeSage, `Bayesian Estimation of Spatial Autoregressive
%             Models',  International Regional Science Review, 1997 
%             Volume 20, number 1\&2, pp. 113-129.
% REFERENCES: LeSage and Pace (2009) Chapter 5 on Bayesian estimation 
%             of spatial regression models.
% For lndet information see: Chapter 4
% For interpretation of direct, indirect and total x-impacts see: Chapter 2
% ---------------------------------------------------

% written by:
% James P. LeSage, last modified 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com



timet = clock;

% error checking on inputs
[n junk] = size(y);
results.y = y;
[n1 k] = size(x);
[n3 n4] = size(W1);
[n5 n6] = size(W2);


if n1 ~= n
error('sac_g: x-matrix contains wrong # of observations');
elseif n3 ~= n4
error('sac_g: W1 matrix is not square');
elseif n3~= n
error('sac_g: W1 matrix is not the same size at y,x');
elseif n5~= n6
error('sac_g: W2 matrix is not the same size at y,x');
elseif n5~= n
error('sac_g: W2 matrix is not the same size at y,x');

end;

% check if the user handled the intercept term okay
    n = length(y);
    if sum(x(:,1)) ~= n
    tst = sum(x); % we may have no intercept term
    ind = find(tst == n); % we do have an intercept term
     if length(ind) > 0
     error('sar: intercept term must be in first column of the x-matrix');
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
    

if nargin == 6
    prior.lflag = 1;
end;

[nu,d0,rval,rho,lambda,sige,rmin,rmax,lmin,lmax,detval1,detval2,ldetflag,eflag,order,iter,novi_flag,c,T,cc1,cc2,a1,a2,inform_flag] = sac_parse(prior,k);


results.order = order;
results.iter = iter;

% error checking on prior information inputs
[checkk,junk] = size(c);
if checkk ~= k
error('sac_g: prior means are wrong');
elseif junk ~= 1
error('sac_g: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('sac_g: prior bcov is wrong');
elseif junk ~= k
error('sac_g: prior bcov is wrong');
end;

P = ones(n,1); 
in = ones(n,1); % initial value for V   
ys = y;
          
vmean = zeros(n,1);
yhat = zeros(n,1);


[rmin,rmax,time1a] = sac_eigs(eflag,W1,rmin,rmax,n);
[lmin,lmax,time1b] = sac_eigs(eflag,W2,lmin,lmax,n);
results.time1 = time1a + time1b;

results.rmin = rmin;
results.rmax = rmax;
results.lmin = lmin;
results.lmax = lmax;

results.lflag = ldetflag;

[detval1,time2a] = sac_lndet(ldetflag,W1,rmin,rmax,detval1,order,iter);
[detval2,time2b] = sac_lndet(ldetflag,W2,lmin,lmax,detval2,order,iter);
results.time2 = time2a+time2b;


% storage for draws
          bsave = zeros(ndraw-nomit,k);
          psave = zeros(ndraw-nomit,1);
          lsave = zeros(ndraw-nomit,1);
          ssave = zeros(ndraw-nomit,1);
          vmean = zeros(n,1);
          acc_rate1 = zeros(ndraw,1);
          acc_rate2 = zeros(ndraw,1);

% ====== initializations
% compute this stuff once to save time
TI = inv(T);
TIc = TI*c;
iter = 1;
in = ones(n,1);
V = in;
Wy = sparse(W1)*y;
Wx = sparse(W1)*x;
vi = in;
P = in;
V = vi;
In = speye(n);

B = speye(n) - lambda*W2;
A = speye(n) - rho*W1;

nu1 = n + 2*nu;   % nu1 is the df of the posterior for sige; n is the # of obs; 


switch (novi_flag) 
    
case{0} % we do heteroscedastic model
    
hwait = waitbar(0,'sac\_g: MCMC sampling ...');
t0 = clock;                  
iter = 1;
acc1 = 0;
acc2 = 0;

   while (iter <= ndraw); % start sampling;
                  
   % update beta 
   xtil = B*x;                            % xtil is used with vi, so not a waste creating here
   ytil = B*A*y;                          % ytil is used with vi, so not a waste creating here
   xstar = matmul(P,xtil);                % P here is the sqrt of inv of covar matrix, nx1
   ystar = P.*ytil;                       % P is all ones when assuming homoscedasticity
   Hinv = inv(xstar'*xstar + sige*TI);    % Hinv is covariance of beta
   b0 = Hinv*(xstar'*ystar + sige*TIc);   % b0 is mean of beta
   bhat = norm_rnd(Hinv) + b0;            % bhat is simulated beta; norm_rnd is MVN, mean 0, covar Hinv
   xb = x*bhat;

            
   % update sige (here we take vi into account)
   Bx = (speye(n) - lambda*W2)*x;
   b = (Bx'*Bx)\(Bx'*B*A*y);
   e = B*(A*y - x*b);
   ev = P.*e;
   d1 = 2*d0 + ev'*ev;
   chi = chis_rnd(1,nu1);
   sige = d1/chi;

   % update vi (based on e, without taking vi into account)
          chiv = chis_rnd(n,rval+1);
          vi = ((e.*e/sige) + in*rval)./chiv;
          P = in./vi; 
              

   % update lambda using metropolis-hastings
          % numerical integration is too slow here
          xb = x*bhat;
          rhox = c_lambda(rho,lambda,y,x,bhat,sige,W1,W2,detval2,P,a1,a2);
          accept = 0;
          lambda2 = lambda + cc1*randn(1,1);
          while accept == 0
           if ((lambda2 > lmin) & (lambda2 < lmax)); 
           accept = 1;  
           else
           lambda2 = lambda + cc1*randn(1,1);
           end; 
          end; 
           rhoy = c_lambda(rho,lambda2,y,x,bhat,sige,W1,W2,detval2,P,a1,a2);
          ru = unif_rnd(1,0,1);
          if ((rhoy - rhox) > exp(1)),
          pp = 1;
          else,          
          ratio = exp(rhoy-rhox);
          pp = min(1,ratio);
          end;
              if (ru < pp)
              lambda = lambda2;
              acc1 = acc1 + 1;
              end;
      acc_rate1(iter,1) = acc1/iter;
      % update cc based on std of rho draws
       if acc_rate1(iter,1) < 0.4
       cc1 = cc1/1.1;
       end;
       if acc_rate1(iter,1) > 0.6
       cc1 = cc1*1.1;
       end;
       
       B = speye(n) - lambda*W2;

       
     % update rho using metropolis-hastings
          % numerical integration is too slow here
          xb = x*bhat;
          rhox = c_rho(rho,lambda,y,x,bhat,sige,W1,W2,detval1,P,a1,a2);
          accept = 0;
          rho2 = rho + cc2*randn(1,1);
          while accept == 0
           if ((rho2 > lmin) & (rho2 < lmax)); 
           accept = 1;  
           else
           rho2 = rho + cc2*randn(1,1);
           end; 
          end; 
           rhoy = c_rho(rho2,lambda,y,x,bhat,sige,W1,W2,detval1,P,a1,a2);
          ru = unif_rnd(1,0,1);
          if ((rhoy - rhox) > exp(1)),
          pp = 1;
          else,          
          ratio = exp(rhoy-rhox);
          pp = min(1,ratio);
          end;
              if (ru < pp)
              rho = rho2;
              acc2 = acc2 + 1;
              end;
      acc_rate2(iter,1) = acc2/iter;
      % update cc based on std of rho draws
       if acc_rate2(iter,1) < 0.4
       cc2 = cc2/1.1;
       end;
       if acc_rate2(iter,1) > 0.6
       cc2 = cc2*1.1;
       end;

       A = speye(n) - rho*W1;

                                                         
    if iter > nomit % if we are past burn-in, save the draws
    bsave(iter-nomit,1:k) = bhat';
    ssave(iter-nomit,1) = sige;
    psave(iter-nomit,1) = rho;
    lsave(iter-nomit,1) = lambda;
    vmean = vmean + vi;        
    end;
                    

iter = iter + 1; 
waitbar(iter/ndraw);         
end; % end of sampling loop
close(hwait);

time3 = etime(clock,t0);
results.time3 = time3;

case{1} % we do homoscedastic model 
    

hwait = waitbar(0,'sac\_g: MCMC sampling ...');
t0 = clock;                  
iter = 1;
acc1 = 0;
acc2 = 0;
          while (iter <= ndraw); % start sampling;
           
   % update beta 
   xtil = B*x;                            % xtil is used with vi, so not a waste creating here
   ytil = B*A*y;                          % ytil is used with vi, so not a waste creating here
   Hinv = inv(xtil'*xtil + sige*TI);      % Hinv is covariance of beta
   b0 = Hinv*(xtil'*ytil + sige*TIc);     % b0 is mean of beta
   bhat = norm_rnd(sige*Hinv) + b0;       % bhat is simulated beta; norm_rnd is MVN, mean 0, covar Hinv
   xb = x*bhat;

            
   % update sige
   Bx = (speye(n) - lambda*W2)*x;
   b = (Bx'*Bx)\(Bx'*B*A*y);
   e = B*(A*y - x*b);
   d1 = 2*d0 + e'*e;
   chi = chis_rnd(1,nu1);
   sige = d1/chi;


   % update lambda using metropolis-hastings
          % numerical integration is too slow here
          xb = x*bhat;
          rhox = c_lambda(rho,lambda,y,x,bhat,sige,W1,W2,detval2,P,a1,a2);
          accept = 0;
          lambda2 = lambda + cc1*randn(1,1);
          while accept == 0
           if ((lambda2 > lmin) & (lambda2 < lmax)); 
           accept = 1;  
           else
           lambda2 = lambda + cc1*randn(1,1);
           end; 
          end; 
           rhoy = c_lambda(rho,lambda2,y,x,bhat,sige,W1,W2,detval2,P,a1,a2);
          ru = unif_rnd(1,0,1);
          if ((rhoy - rhox) > exp(1)),
          pp = 1;
          else,          
          ratio = exp(rhoy-rhox);
          pp = min(1,ratio);
          end;
              if (ru < pp)
              lambda = lambda2;
              acc1 = acc1 + 1;
              end;
      acc_rate1(iter,1) = acc1/iter;
      % update cc based on std of rho draws
       if acc_rate1(iter,1) < 0.4
       cc1 = cc1/1.1;
       end;
       if acc_rate1(iter,1) > 0.6
       cc1 = cc1*1.1;
       end;
       
       B = speye(n) - lambda*W2;

       
     % update rho using metropolis-hastings
          % numerical integration is too slow here
          xb = x*bhat;
          rhox = c_rho(rho,lambda,y,x,bhat,sige,W1,W2,detval1,P,a1,a2);
          accept = 0;
          rho2 = rho + cc2*randn(1,1);
          while accept == 0
           if ((rho2 > lmin) & (rho2 < lmax)); 
           accept = 1;  
           else
           rho2 = rho + cc2*randn(1,1);
           end; 
          end; 
           rhoy = c_rho(rho2,lambda,y,x,bhat,sige,W1,W2,detval1,P,a1,a2);
          ru = unif_rnd(1,0,1);
          if ((rhoy - rhox) > exp(1)),
          pp = 1;
          else,          
          ratio = exp(rhoy-rhox);
          pp = min(1,ratio);
          end;
              if (ru < pp)
              rho = rho2;
              acc2 = acc2 + 1;
              end;
      acc_rate2(iter,1) = acc2/iter;
      % update cc based on std of rho draws
       if acc_rate2(iter,1) < 0.4
       cc2 = cc2/1.1;
       end;
       if acc_rate2(iter,1) > 0.6
       cc2 = cc2*1.1;
       end;

       A = speye(n) - rho*W1;

                         

    if iter > nomit % if we are past burn-in, save the draws
    bsave(iter-nomit,1:k) = bhat';
    ssave(iter-nomit,1) = sige;
    psave(iter-nomit,1) = rho;
    lsave(iter-nomit,1) = lambda;
    vmean = vmean + vi;
    end;
                    
iter = iter + 1; 
waitbar(iter/ndraw);         
end; % end of sampling loop
close(hwait);

time3 = etime(clock,t0);
results.time3 = time3;
% ===============================================================


otherwise
error('sac_g: unrecognized novi_flag value on input');
% we should never get here

end; % end of homoscedastic vs. heteroscedastic options

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
    wjjju=W1*wjjju;
    tracew(jjj)=mean(mean(rv.*wjjju));
    
end

traces=[tracew];
traces(1)=0;
traces(2)=sum(sum(W1'.*W1))/nobs;
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

% compute posterior means 
vmean = vmean/(ndraw-nomit);
bmean = mean(bsave);
bmean = bmean';
rho = mean(psave);
lam = mean(lsave);
sige = mean(ssave);


B = speye(n) - lam*W2;
A = speye(n) - rho*W1;
Bx = (speye(n) - lam*W2)*x;
b = (Bx'*Bx)\(Bx'*B*A*y);
e = B*(A*y - x*b);
ev = P.*e;

results.resid = e;
results.yhat = y-e;

[nobs,nvar] = size(x);

sigu = e'*e;
sige = sigu/(nobs-nvar);
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % conventional r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);

time = etime(clock,timet);

results.meth  = 'sac_g';
results.beta = bmean;
results.rho = rho;
results.lambda = lam;
results.sige = sige;
results.bdraw = bsave;
results.pdraw = psave;
results.ldraw = lsave;
results.sdraw = ssave;
results.vmean = vmean;
results.total = total;
results.direct = direct;
results.indirect = indirect;
results.yhat  = yhat;
results.bmean = c;
results.bstd  = sqrt(diag(T));
results.rsqr  = rsqr;
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
results.sige = sige;
results.nobs  = n;
results.nvar  = nvar;
results.ndraw = ndraw;
results.nomit = nomit;
results.time  = time;
results.acc1 = acc_rate1;
results.acc2 = acc_rate2;
results.nu = nu;
results.d0 = d0;
results.a1 = a1;
results.a2 = a2;
results.tflag = 'plevel';
results.novi = novi_flag;
results.lndet1 = detval1;
results.lndet2 = detval2;
results.priorb = inform_flag;



% =========================================================================
% support functions are below
% =========================================================================


function cout = c_lambda(rho,lambda,y,x,b,sige,W1,W2,detval,P,a1,a2);
% PURPOSE: evaluate the conditional distribution of lambda given other
%          parameters using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:cout = c_lambda(lambda,rho,y,x,b,sige,W1,W2,detval,P,a1,a2)
%  where:  rho  = spatial autoregressive parameter
%        lambda = spatial error parameter
%          y    = dependent variable vector
%          W1    = spatial lag weight matrix
%          W2    = spatial error weight matrix
%        detval = an (ngrid,2) matrix of values for det(I-lambda*W2) 
%                 over a grid of lambda values 
%                 detval(:,1) = determinant values
%                 detval(:,2) = associated lambda values
%          sige = sige value
%             P = vector of vi-values
%          a1    = (optional) prior parameter for rho
%          a2    = (optional) prior parameter for rho
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by sac_g
%  --------------------------------------------------

gsize = detval(2,1) - detval(1,1);
% Note these are actually log detvalues
i1 = find(detval(:,1) <= lambda + gsize);
i2 = find(detval(:,1) <= lambda - gsize);
i1 = max(i1);
i2 = max(i2);
index = round((i1+i2)/2);
if isempty(index)
index = 1;
end;
detm = detval(index,2); 

[n,k] = size(x);
nmk = (n-k)/2;

B = speye(n) - lambda*W2;
A = speye(n) - rho*W1;
Bx = B*x;
b = (Bx'*Bx)\(Bx'*B*A*y);
   e = B*(A*y - x*b);
   ev = sqrt(P).*e;

epe = (ev'*ev)/(2*sige);

cout =   detm - epe;


function cout = c_rho(rho,lambda,y,x,b,sige,W1,W2,detval,P,a1,a2);
% PURPOSE: evaluate the conditional distribution of rho given other
%          parameters using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:cout = c_rho(rho,lambda,y,x,b,sige,W1,W2,detval,P,a1,a2)
%  where:  rho  = spatial autoregressive parameter, candidate value
%        lambda = spatial error parameter
%          y    = dependent variable vector
%          x    = explanatory variables matrix
%          W1    = spatial lag weight matrix
%          W2    = spatial error weight matrix
%        detval = an (ngrid,2) matrix of values for det(I-lambda*W2) 
%                 over a grid of lambda values 
%                 detval(:,1) = determinant values
%                 detval(:,2) = associated lambda values
%             P = vector of vi-values
%          sige = sige value
%          a1    = (optional) prior parameter for rho
%          a2    = (optional) prior parameter for rho
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by sac_g
%  --------------------------------------------------

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

[n,k] = size(x);
nmk = (n-k)/2;

B = speye(n) - lambda*W2;
A = speye(n) - rho*W1;
Bx = B*x;
b = (Bx'*Bx)\(Bx'*B*A*y);

   e = B*(A*y - x*b);
   ev = sqrt(P).*e;
   
epe = (ev'*ev)/(2*sige);

cout =   detm - epe;



function [nu,d0,rval,rho,lambda,sige,rmin,rmax,lmin,lmax,detval1,detval2,ldetflag,eflag,order,iter,novi_flag,c,T,cc1,cc2,a1,a2,inform_flag] = sac_parse(prior,k)
% PURPOSE: parses input arguments for sac_g models
% ---------------------------------------------------
%  USAGE: [nu,d0,rval,mm,kk,rho,lambda,sige,rmin,rmax,lmin,lmax,detval1, detval2, ...
%          ldetflag,eflag,order,iter,novi_flag,c,T,prior_beta,cc1,cc2,a1,a2,inform_flag] = 
%                           sac_parse(prior,k)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% set defaults

eflag = 1;     % default to not computing eigenvalues
ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
order = 50;    % there are parameters used by the MC det approx
iter = 30;     % defaults based on Pace and Barry recommendation
rmin = -1;     % use -1,1 rho interval as default
rmax = 1;
lmin = -1;
lmax = 1;
detval1 = 0;    % just a flag
detval2 = 0;
rho = 0.5;
lambda = 0.2;
sige = 1.0;
rval = 4;
nu = 0;
d0 = 0;
a1 = 1.0;
a2 = 1.0;
c = zeros(k,1);   % diffuse prior for beta
T = eye(k)*1e+12;
novi_flag = 0;    % default is do vi-estimates
cc1 = 0.2;         % initial tuning parameter for M-H sampling
cc2 = 0.2;
inform_flag = 0;  % flag for diffuse prior on beta

fields = fieldnames(prior);
nf = length(fields);
if nf > 0
 for i=1:nf
    if strcmp(fields{i},'nu')
        nu = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;  
    elseif strcmp(fields{i},'rval')
        rval = prior.rval; 
    elseif strcmp(fields{i},'eigs')
        eflag = prior.eigs;
    elseif strcmp(fields{i},'a1')
       a1 = prior.a1; 
    elseif strcmp(fields{i},'a2')
       a2 = prior.a2;  
    elseif strcmp(fields{i},'beta')
        c = prior.beta; inform_flag = 1; % flag for informative prior on beta
    elseif strcmp(fields{i},'bcov')
        T = prior.bcov; inform_flag = 1; % flag for informative prior on beta
    elseif strcmp(fields{i},'rmin')
        rmin = prior.rmin;  
    elseif strcmp(fields{i},'rmax')
        rmax = prior.rmax; 
    elseif strcmp(fields{i},'lmin')
        lmin = prior.lmin;  
    elseif strcmp(fields{i},'lmax')
        lmax = prior.lmax; 
    elseif strcmp(fields{i},'lndet')
    detval1 = prior.lndet1;
    detval2 = prior.lndet2;
    ldetflag = -1;
    rmin = detval1(1,1);
    nr = length(detval1);
    rmax = detval1(nr,1);
    lmin = detval2(1,1);
    nl = length(detval2);
    lmax = detval(nl,1);
    lmin = detval2(1,1);
    elseif strcmp(fields{i},'lflag')
        tst = prior.lflag;
        if tst == 0,
        ldetflag = 0; 
        elseif tst == 1,
        ldetflag = 1; 
        elseif tst == 2,
        ldetflag = 2; 
        else
        error('sac_g: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = prior.order;  
    elseif strcmp(fields{i},'iter')
        iter = prior.iter; 
    elseif strcmp(fields{i},'novi')
        novi_flag = prior.novi;
    end;
 end;
 
else, % the user has input a blank info structure
      % so we use the defaults
end; 


function [rmin,rmax,time2] = sac_eigs(eflag,W,rmin,rmax,n);
% PURPOSE: compute the eigenvalues for the weight matrix
% ---------------------------------------------------
%  USAGE: [rmin,rmax,time2] = sac_eigs(eflag,W,rmin,rmax,W)
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


function [detval,time1] = sac_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
% PURPOSE: compute the log determinant |I_n - rho*W|
% using the user-selected (or default) method
% ---------------------------------------------------
%  USAGE: detval = sac_lndet(lflag,W,rmin,rmax,detval,order,iter)
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
            error('sac_g: wrgon lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('sac_g: wrong sized lndet input argument');
        elseif n1 == 1
            error('sac_g: wrong sized lndet input argument');
        end;          
end;

