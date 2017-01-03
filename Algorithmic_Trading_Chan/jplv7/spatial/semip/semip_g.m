function results = semip_g(y,x,W,m,mobs,ndraw,nomit,prior)
% PURPOSE: Bayesian Probit model with individual effects exhibiting spatial dependence:
%	      Y = (Yi, i=1,..,m) with each vector, Yi = (yij:j=1..Ni) consisting of individual 
%          dichotomous observations in regions i=1..m, as defined by yij = Indicator(zij>0), 
%          where latent vector Z = (zij) is given by the linear model:
%
%          Z = x*b + del*a + e   with:
%
%          x = n x k matrix of explanatory variables [n = sum(Ni: i=1..m)]; 
%			  del = n x m indicator matrix with del(j,i) = 1 iff indiv j is in reg i;
%          a = (ai: i=1..m) a vector of random regional effects modeled by
%
%          a = rho*W*a + U,     U ~ N[0,sige*I_m] ; (I_m = m-square Identity matrix)
%
%          and with e ~ N(0,V), V = diag(del*v) where v = (vi:i=1..m). 
%
%          The priors for the above parameters are of the form:
%          r/vi ~ ID chi(r), r ~ Gamma(m,k)
%          b ~ N(c,T),  
%          1/sige ~ Gamma(nu,d0), 
%          rho ~ beta(a1,a2)
%-----------------------------------------------------------------
% USAGE: results = semip_g(y,x,W,m,mobs,ndraw,nomit,prior)
% where: y = dependent variable vector (nobs x 1) [must be zero-one]
%        x = independent variables matrix (nobs x nvar)
%        W = 1st order contiguity matrix (standardized, row-sums = 1)
%        m = # of regions 
%     mobs = an m x 1 vector containing the # of observations in each
%            region [= (Ni:i=1..m) above]
%    ndraw = # of draws
%    nomit = # of initial draws omitted for burn-in            
%    prior = a structure variable with:
%            prior.beta  = prior means for beta,  (= c above) 
%                          (default = 0)
%            prior.bcov  = prior beta covariance , (= T above)  
%                          [default = 1e+12*I_k ]
%            prior.rval  = r prior hyperparameter, default=4
%            prior.a1    = parameter for beta(a1,a2) prior on rho (default = 1.01)
%            prior.a2    = (default = 1.01) see: 'help beta_prior'
%            prior.m     = informative Gamma(m,k) prior on r
%            prior.k     = (default: not used)
%            prior.nu    = informative Gamma(nu,d0) prior on sige
%            prior.d0    = default: nu=0,d0=0 (diffuse prior)
%            prior.rmin  = (optional) min rho used in sampling (default = -1)
%            prior.rmax  = (optional) max rho used in sampling (default = 1)  
%            prior.lflag = 0 for full lndet computation (default = 1, fastest)
%                        = 1 for MC approx (fast for large problems)
%                        = 2 for Spline approx (medium speed)
%            prior.dflag = 0 for numerical integration, 1 for Metropolis-Hastings (default = 0)
%            prior.eig   = 0 for default rmin = -1,rmax = +1, 1 for eigenvalue calculation of these
%            prior.order = order to use with prior.lflag = 1 option (default = 50)
%            prior.iter  = iters to use with prior.lflag = 1 option (default = 30)   
%---------------------------------------------------
% RETURNS:  a structure:
%          results.meth   = 'semip_g'
%          results.bdraw  = bhat draws (ndraw-nomit x nvar)
%          results.pdraw  = rho  draws (ndraw-nomit x 1)
%          results.adraw  = a draws (ndraw-nomit x m)
%          results.amean  = mean of a draws (m x 1)
%          results.sdraw  = sige draws (ndraw-nomit x 1)
%          results.vmean  = mean of vi draws (m x 1) 
%          results.rdraw  = r draws (ndraw-nomit x 1) (if m,k input)
%          results.bmean  = b prior means, prior.beta from input
%          results.bstd   = b prior std deviations sqrt(diag(prior.bcov))
%          results.r      = value of hyperparameter r (if input)
%          results.rsqr   = R-squared
%          results.nobs   = # of observations
%          results.mobs   = mobs vector from input
%          results.nreg   = # of regions
%          results.nvar   = # of variables in x-matrix
%          results.ndraw  = # of draws
%          results.nomit  = # of initial draws omitted
%          results.y      = actual (0,1) observations (nobs x 1)
%          results.zmean  = mean of latent z-draws (nobs x 1)
%          results.yhat   = mean of posterior y-predicted (nobs x 1)
%          results.nu     = nu prior parameter
%          results.d0     = d0 prior parameter
%          results.time1  = time for eigenvalue calculation
%          results.time2  = time for log determinant calcluation
%          results.time3  = time for sampling
%          results.time   = total time taken 
%          results.rmax   = 1/max eigenvalue of W (or rmax if input)
%          results.rmin   = 1/min eigenvalue of W (or rmin if input)          
%          results.tflag  = 'plevel' (default) for printing p-levels
%                         = 'tstat' for printing bogus t-statistics 
%          results.rflag  = 1, if a beta(a1,a2) prior for rho, 0 otherwise
%          results.lflag  = lflag from input
%          results.dflag  = dflag from input
%          results.bflag  = 1 for informative prior on beta, 0 otherwise
%          results.iter   = prior.iter  option from input
%          results.order  = prior.order option from input
%          results.limit  = matrix of [rho lower95,logdet approx, upper95] 
%                           intervals for the case of lflag = 1 
%          results.acc   = acceptance rate for M-H sampling (ndraw x 1 vector)
% ----------------------------------------------------
% SEE ALSO: sem_gd, prt, semp_g, coda
% ----------------------------------------------------
% REFERENCES: Tony E. Smith "A Bayesian Probit Model with Spatial Dependencies" unpublished manuscript
% For lndet information see: Ronald Barry and R. Kelley Pace, "A Monte Carlo Estimator
% of the Log Determinant of Large Sparse Matrices", Linear Algebra and
% its Applications", Volume 289, Number 1-3, 1999, pp. 41-54.
% and: R. Kelley Pace and Ronald P. Barry "Simulating Mixed Regressive
% Spatially autoregressive Estimators", Computational Statistics, 1998,
% Vol. 13, pp. 397-418.
%----------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% NOTE: much of the speed for large problems comes from:
% the use of methods pioneered by Pace and Barry.
% R. Kelley Pace was kind enough to provide functions
% lndetmc, and lndetint from his spatial statistics toolbox
% for which I'm very grateful.

timet = clock;

time1 = 0;
time2 = 0;
time3 = 0;

% error checking on inputs
[n junk] = size(y);
results.y = y;
[n1 k] = size(x);
[n3 n4] = size(W);

if n1 ~= n
error('semip_g: x-matrix contains wrong # of observations');
elseif n3 ~= n4
error('semip_g: W matrix is not square');
elseif n3~= m
error('semip_g: W matrix is not the same size as # of regions');
end;

% check that mobs vector is correct
obs_chk = sum(mobs);
if obs_chk ~= n
error('semip_g: wrong # of observations in mobs vector');
end;
if length(mobs) ~= m
error('semip_g: wrong size mobs vector -- should be m x 1');
end;

[nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,c,T,inform_flag,cc,metflag,a1,a2] = semip_parse(prior,k);

% error checking on prior information inputs
[checkk,junk] = size(c);
if checkk ~= k
error('sar_g: prior means are wrong');
elseif junk ~= 1
error('sar_g: prior means are wrong');
end;

[checkk junk] = size(T);
if checkk ~= k
error('sar_g: prior bcov is wrong');
elseif junk ~= k
error('sar_g: prior bcov is wrong');
end;

results.order = order;
results.iter = iter;

[rmin,rmax,time1] = semip_eigs(eflag,W,rmin,rmax,m);

[detval,time2] = semip_lndet(ldetflag,W,rmin,rmax,0,order,iter);


rv = detval(:,1);
nr = length(rv);


% storage for draws
          bsave = zeros(ndraw-nomit,k);
          asave = zeros(ndraw-nomit,m);
          if mm~= 0
          rsave = zeros(ndraw-nomit,1);
          end;
          ssave = zeros(ndraw-nomit,1);
          psave = zeros(ndraw-nomit,1);
          vmean = zeros(m,1);
          amean = zeros(m,1);
          zmean = zeros(n,1);
          yhat = zeros(n,1);
          acc_rate = zeros(ndraw,1);

% ====== initializations
% compute this once to save time
TI = inv(T);
TIc = TI*c;
iter = 1;
inV0 = ones(n,1);   % default starting value for inV [= inv(V)]
a0 = ones(m,1);
z0 = y; %default starting value for latent vector z
a = a0;
z = z0;
in = ones(n,1);
inV = inV0;
vi = ones(m,1);
tvec = ones(n,1); % initial values
evec = ones(m,1);
b1 = ones(m,1);
Bp = speye(m) - rho*sparse(W);

%Computations for updating vector a

if(m > 100)
W1 = zeros(m,m-1);
W2 = zeros(m-1,m);
W3 = zeros(m,m-1);

for i = 1:m
   w1(i) = W(:,i)'*W(:,i);
   if i == 1
      W1(1,:) = W(1,[2:m]);            %W-rows minus entry i
      W2(:,1) = W([2:m],1);            %W-columns minus entry i
      W3(1,:) = W(:,1)'*W(:,[2:m]);
   elseif i == m
      W1(m,:) = W(m,[1:m-1]);
      W2(:,m) = W([1:m-1],m);
      W3(m,:) = W(:,m)'*W(:,[1:m-1]);
   else
      W1(i,:) = W(i,[1:i-1,i+1:m]);
      W2(:,i) = W([1:i-1,i+1:m],i);
      W3(i,:) = W(:,i)'*W(:,[1:i-1,i+1:m]);
   end
end

end %end if(m > 10)
    
%*********************************
% START SAMPLING
%*********************************

dmean = zeros(length(detval),1);

hwait = waitbar(0,'MCMC sampling ...');
t0 = clock;                  
iter = 1;
acc = 0;
cc = 0.1;
cntr = 1;
          while (iter <= ndraw); % start sampling;

          
          % UPDATE: beta   
          xs = matmul(sqrt(inV),x);
          zs = sqrt(inV).*z; 
          A0i = inv(xs'*xs + TI);
          zmt = sqrt(inV).*(z-tvec);
                   
          b = xs'*zmt + TIc;
          b0 = A0i*b;
          bhat = norm_rnd(A0i) + b0; 
          %Update b1
          e0 = z - x*bhat;
          cobs = 0;
          for i=1:m;
            obs = mobs(i,1);
            b1(i,1) = sum(e0(cobs+1:cobs+obs,1)/vi(i,1));
            cobs = cobs + obs;
           end;

                             
          % UPDATE: a 
          
          if m <= 100   %Procedure for small m 
             vii = ones(m,1)./vi;
             A1i = inv((1/sige)*Bp'*Bp + diag(vii.*mobs));
             a = norm_rnd(A1i) + A1i*b1;             
             
           else   %Procedure for large m
             
             cobs = 0;
             
             for i = 1:m
                
                obs = mobs(i,1); 
                
                if i == 1            %Form complementary vector
                   ai = a(2:m);
                elseif i == m
                   ai = a(1:m-1);
                else
                   ai = a([1:i-1,i+1:m]);
                end                
                
                                
                di = (1/sige) + ((rho^2)/sige)*w1(i) + (obs/vi(i));                
                               
                zi = z(cobs+1:cobs+obs,1);                
                xbi = x([cobs+1:cobs+obs],:)* bhat;                
                phi = (1/vi(i))*(ones(1,obs)*(zi - xbi));  
                awi = ai'*(W1(i,:)' + W2(:,i));                
                bi = phi + (rho/sige)*awi - ((rho^2)/sige)*(W3(i,:)*ai) ;                
                a(i) = (bi/di) + sqrt(1/di)*randn(1,1);                                              
                cobs = cobs + obs;
                
             end %end for i = 1:m 
             
          end %end if on m
          
          % Update tvec = del*a   
             
          cobs = 0;              
          for i=1:m;
             obs = mobs(i,1);
             tvec(cobs+1:cobs+obs,1) = a(i,1)*ones(obs,1);
             cobs = cobs + obs;
          end;
          
                  
			 % UPDATE: sige

          term1 = a'*Bp'*Bp*a + 2*d0;
          chi = chis_rnd(1,m + 2*nu);
          sige = term1/chi; 
          
          
			 % UPDATE: vi (and form inV, b1)

           e = z - x*bhat - tvec;         
                                
           cobs = 0;
           for i=1:m;
            obs = mobs(i,1);
            ee = e(cobs+1:cobs+obs,1)'*e(cobs+1:cobs+obs,1);
            chi = chis_rnd(1,rval+obs);
            vi(i,1) = (ee + rval)/chi; 
            inV(cobs+1:cobs+obs,1) = ones(obs,1)/vi(i,1);
            b1(i,1) = sum(e0(cobs+1:cobs+obs,1)/vi(i,1));
            cobs = cobs + obs;
           end;
    
          % UPDATE: rval (if necessary)
           
          if mm ~= 0           
          rval = gamm_rnd(1,1,mm,kk);  
          end;

         if (metflag == 1) | (inform_flag == 1)
          % UPDATE: rho (using metropolis step)
          % Construct new candidate rho value: rho2
             %  Using proposal distribution: N(rho,cc^2) 
             %  truncated to the interval (rmin,rmax) 
          accept = 0;
          while accept == 0
             rho2 = rho + cc*randn(1,1);    
             if (rmin < rho2 & rho2 < rmax)
              accept = 1;
            end;
            cntr = cntr+1; % counts acceptance rate
          end;
          
          %Form density ratio
          
          rhox = c_semip(rho,a,sige,W,detval,a1,a2);  %log density at rho
          rhoy = c_semip(rho2,a,sige,W,detval,a1,a2); %log density at rho2
          ratio = exp(rhoy-rhox);
          
          %Make Metropolis comparison
                                    
          if ratio > 1,
          	p = 1;
          else,
            p = min(1,ratio);
          end;
         
          ru = unif_rnd(1,0,1);
          if (ru < p)
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

      end; % end of if metflag == 1

      if (metflag == 0) & (inform_flag == 0)

          % UPDATE: rho (using univariate integration)
          C0 = a'*a;
          Wa = W*a;
          C1 = a'*Wa;
          C2 = Wa'*Wa;
          
		  rho = draw_rho(detval,C0,C1,C2,sige,rho,a1,a2);
end; % end  sampling for rho
 
	  	  Bp = speye(m) - rho*sparse(W); 

   % UPDATE: z

      lp = x*bhat + tvec;
	       ind = find(y == 0);
		    tmp = ones(n,1)./inV;
          z(ind,1) = normrt_rnd(lp(ind,1),tmp(ind,1),0);
          %z(ind,1) = rtanorm_combo(lp(ind,1),tmp(ind,1),zeros(length(ind),1));
	       ind = find(y == 1);
          z(ind,1) = normlt_rnd(lp(ind,1),tmp(ind,1),0);
          %z(ind,1) = rtbnorm_combo(lp(ind,1),tmp(ind,1),zeros(length(ind),1));

    if iter > nomit % if we are past burn-in, save the draws
    	bsave(iter-nomit,1:k) = bhat';
    	asave(iter-nomit,1:m) = a'; % should just return the mean
    	ssave(iter-nomit,1) = sige;
    	psave(iter-nomit,1) = rho;
    	amean = amean + a;
    	yhat = yhat + lp;
    	zmean = zmean + z;
    	vmean = vmean + vi;

    	if mm~= 0
        rsave(iter-nomit,1) = rval;
     	end;
    end;
    
           
    iter = iter + 1;
       
waitbar(iter/ndraw);     

end; % end of sampling loop

close(hwait);

time3 = etime(clock,t0);

vmean = vmean/(ndraw-nomit);
yhat = yhat/(ndraw-nomit);
amean = amean/(ndraw-nomit);
zmean = zmean/(ndraw-nomit);


time = etime(clock,timet);

results.meth  = 'semip_g';
results.bdraw = bsave;
results.adraw = asave;
results.pdraw = psave;
results.sdraw = ssave;
results.vmean = vmean;
results.amean = amean;
results.yhat  = yhat;
results.zmean = zmean;
results.bmean = c;
results.bstd  = sqrt(diag(T));
results.bflag = inform_flag;
results.dflag = metflag;
results.eflag = eflag;
results.nobs  = n;
results.nvar  = k;
results.ndraw = ndraw;
results.nomit = nomit;
results.time = time;
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.nu = nu;
results.d0 = d0;
results.tflag = 'plevel';
results.lflag = ldetflag;
results.nreg = m;
results.mobs = mobs;
results.acc = acc_rate;
results.rmin = rmin;
results.rmax = rmax;
results.a1 = a1;
results.a2 = a2;


if mm~= 0
results.rdraw = rsave;
results.m     = mm;
results.k     = kk;
else
results.r     = rval;
results.rdraw = 0;
end;



function cout = c_semip(rho,a,sige,W,detval,a1,a2)
% PURPOSE: evaluate the conditional distribution of rho for semip model
% ---------------------------------------------------
%  USAGE:cout = c_semip(rho,a,W,detval,sige,a1,a2)
%  where:  rho  = spatial autoregressive parameter
%          a    = regional effects vector
%          W    = spatial weight matrix
%        detval = an (ngrid,2) matrix of values for det(I-rho*W) 
%                 over a grid of rho values 
%                 detval(:,1) = rho values
%                 detval(:,2) = associated log det values
%          a1    = optional beta prior for rho
%          a2    = optional beta prior for rho
% ---------------------------------------------------
%  RETURNS: a conditional used in Metropolis-Hastings sampling
%  NOTE: called only by semip_g
%  --------------------------------------------------

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

gsize = detval(2,1) - detval(1,1);
i1 = find(detval(:,1) <= rho + gsize);
i2 = find(detval(:,1) <= rho - gsize);
i1 = max(i1);
i2 = max(i2);
index = round((i1+i2)/2);
if isempty(index)
index = 1;
end;
detm = detval(index,2);
if nargin == 7
bprior = beta_prior(detval(:,1),a1,a2);
detm = detm + log(bprior);
end;
n = length(a);
z = speye(n) - rho*sparse(W);
epe = (a'*z'*z*a)/(2*sige);
cout = detm - epe;

function rho = draw_rho(detval,epe0,eped,epe0d,sige,rho,a1,a2)
% update rho via univariate numerical integration

nrho = length(detval(:,1));
iota = ones(nrho,1);

z = epe0*iota - 2*detval(:,1)*eped + detval(:,1).*detval(:,1)*epe0d;
den = detval(:,2) - (z/2*sige);
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

rtmp = unif_rnd(1,0,1);
rnd = rtmp*sum(z);
ind = find(den <= rnd);
idraw = max(ind);
if (idraw > 0 & idraw < nrho)
rho = detval(idraw,1);
end;

function [rmin,rmax,time2] = semip_eigs(eflag,W,rmin,rmax,n);
% PURPOSE: compute the eigenvalues for the weight matrix
% ---------------------------------------------------
%  USAGE: [rmin,rmax,time2] = semip_eigs(eflag,W,rmin,rmax,W)
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



function [nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval,ldetflag,eflag,order,iter,c,T,inform_flag,cc,metflag,a1,a2] = semip_parse(prior,k)
% PURPOSE: parses input arguments 
% ---------------------------------------------------
%  USAGE: [nu,d0,rval,mm,kk,rho,sige,rmin,rmax,detval, ...
%         ldetflag,eflag,order,iter,novi_flag,c,T,prior_beta,cc,metflag],a1,a2 = 
%                           semip_parse(prior,k)
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
sige = 1.0;
rval = 4;
mm = 0;
kk = 0;
nu = 0;
d0 = 0;
a1 = 1.01;
a2 = 1.01;
c = zeros(k,1);   % diffuse prior for beta
T = eye(k)*1e+12;
prior_beta = 0;   % flag for diffuse prior on beta
cc = 0.2;
metflag = 0; % use integration instead of M-H sampling for rho
inform_flag = 0;

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
    elseif strcmp(fields{i},'dflag')
       metflag = prior.dflag; 
    elseif strcmp(fields{i},'a1')
       a1 = prior.a1; 
    elseif strcmp(fields{i},'a2')
       a2 = prior.a2; 
    elseif strcmp(fields{i},'m')
        mm = prior.m;
        kk = prior.k;
        rval = gamm_rnd(1,1,mm,kk);    % initial value for rval   
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
        error('sar_g: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = prior.order;  
    elseif strcmp(fields{i},'iter')
        iter = prior.iter; 
    elseif strcmp(fields{i},'dflag')
        metflag = prior.dflag;
    elseif strcmp(fields{i},'eig')
        eflag = prior.eig;
    end;
 end;

 
else, % the user has input a blank info structure
      % so we use the defaults
end; 


function [detval,time1] = semip_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
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
            error('sar_g: wrong lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('sar_g: wrong sized lndet input argument');
        elseif n1 == 1
            error('sar_g: wrong sized lndet input argument');
        end;          
end;

