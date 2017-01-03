function results = ols_gv(y,x,ndraw,nomit,prior)
% PURPOSE: MCMC estimates for the Bayesian heteroscedastic linear model
%          y = X B + e, p(e_i) = f_t(e_i | 0, v_i, lambda)
%          p(V) = diag(v_1,v_2,...,v_n) = prod f_G(1/v_i | 1, lambda)  
%          p(lambda) = f_G(lambda | v0, 2)
%          p(B) = f_N(B | c,T),  
%          p(h) = f_G(h | nu, d0) 
%---------------------------------------------------
% USAGE: results = ols_g(y,x,ndraw,nomit,prior,start)
% where: y    = dependent variable vector
%        x    = independent variables matrix of rank(k)
%       ndraw = # of draws
%       nomit = # of initial draws omitted for burn-in
%       prior = a structure for prior information input
%               prior.beta, prior means for beta,   c above (default diffuse)
%               priov.bcov, prior beta covariance , T above (default diffuse)
%               prior.nu,   prior parameter, f_G(h | nu, d0), default = 1
%               prior.d0,   prior parameter, default = 1e-8
%               prior.v0,   prior parameter, f_G(lambda | v0, 2), default = 15
% ---------------------------------------------------
% RETURNS: a structure:
%          results.meth  = 'ols_gv'
%          results.bdraw = bhat draws (ndraw-nomit x nvar)
%          results.vmean = mean of vi draws (nobs x 1)
%          results.sdraw = sige draws (ndraw-nomit x 1)
%          results.rdraw = dof parameter lambda draws
%          results.yhat  = mean of draws from posterior for y-predicted
%          results.pmean = b prior means (prior.beta from input)
%          results.pstd  = b prior std deviation, sqrt(prior.bcov)
%          results.rmean = posterior mean of dof parameter lambda
%          results.rmedian = posterior median of dof parameter lambda
%          results.v0    = prior v0-value for lambda prior
%          results.nu    = prior nu-value for sige prior
%          results.d0    = prior d0-value for sige prior
%          results.nobs  = # of observations
%          results.nvar  = # of variables
%          results.ndraw = # of draws
%          results.nomit = # of initial draws omitted
%          results.y     = actual observations
%          results.x     = x-matrix
%          results.time  = time taken for sampling
%          results.hpd   =  95 percent hpd intervals
%          results.acc   = acceptance rate
%---------------------------------------------------
% REFERENCES: Geweke (1993)  'Bayesian Treatment of the 
% Independent Student-$t$ Linear Model', Journal of Applied
% Econometrics, 8, s19-s40.
% Gary Koop, Bayesian Econometrics (2003), chapter 6, section 6.4
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


% default Hyperparameters for independent Normal-Gamma prior
[n,k] = size(x);

b0= zeros(k,1);
capv0=(10000^2)*eye(k);
capv0inv=inv(capv0);
%Prior for degrees of freedom is exponential with mean vl0
v0=15;
vdraw=v0; % initial value for lambda

%Ordinary least squares quantities
nu0 = 1;
d0 = 1e-8;
v1=d0+n;
v0d0 = nu0*d0;
v1 = nu0 + n;


if nargin == 5
fields = fieldnames(prior);
nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'v0')
        v0 = prior.v0; 
    elseif strcmp(fields{i},'nu')
        nu0 = prior.nu;
    elseif strcmp(fields{i},'d0')
        d0 = prior.d0;   
    elseif strcmp(fields{i},'beta');
    b0 = prior.beta;
    elseif strcmp(fields{i},'bcov');
    T = prior.bcov;
    capv0inv = inv(T);
    end;
 end;
end;

v1 = nu0 + n;
vs = nu0*d0;



%Now start things for Gibbs loop
%candidate generating density for dof is Normal with mean = oldraw
%and variance vscale
postvar=1;
cc= 5;
vscale= cc*postvar;
in = ones(n,1);
%store all draws in the following matrices
%initialize them here
b_=[];
h_=[];
v_=[];
vmean = zeros(n,1);
pswitch=0;


%Specify the number of replications
%number of burnin replications
s0 = nomit;
%number of retained replications
s1=ndraw;
s=s0+s1;
pswitch=0;
acc_rate = zeros(s,1);
ccsave = zeros(s,1);
%choose a starting value for h and pdraw
%pdraw is the matrix used for transformation
hdraw=1;
lamdraw=ones(n,1);
pdraw=ones(n,1);
vi = ones(n,1);
   
hwait = waitbar(0,'ols\_gv: MCMC sampling ...');
acc = 0;
iota = ones(n,1);

for i = 1:s
    ystar=pdraw.*y;
    xstar=matmul(pdraw,x);
    xsquare=xstar'*xstar;
    
    %draw from beta conditional on rest
    capv1inv = capv0inv + hdraw*xsquare;
    capv1=inv(capv1inv);
    b1 = capv1*(capv0inv*b0 + hdraw*xstar'*ystar);
    bdraw = b1 + norm_rnd(capv1);
     
    %draw from h conditional on rest
    sbar = ((ystar-xstar*bdraw)'*(ystar-xstar*bdraw) + vs);
    chi = chis_rnd(1,v1);
    hdraw = chi/sbar;


    %Random walk Metropolis step for dof
    temp = -log(lamdraw) + lamdraw;
    nu = 1/v0 + .5*sum(temp);
    
     vlcan= vdraw +  cc*randn(1,1);
     if vlcan>0
        lpostcan = .5*n*vlcan*log(.5*vlcan) -n*gammaln(.5*vlcan)...
        -nu*vlcan;
        lpostdraw = .5*n*vdraw*log(.5*vdraw) -n*gammaln(.5*vdraw)...
        -nu*vdraw;
        accprob = exp(lpostcan-lpostdraw);
     else
        accprob=0;
     end
     

%accept candidate draw with log prob = laccprob, else keep old draw
   if  rand<accprob
       vdraw = vlcan;
       pswitch = pswitch + 1;
       acc = acc + 1;
   end    
    
      acc_rate(i,1) = acc/i;
      % update cc based on std of rho draws
       if acc_rate(i,1) < 0.4
       cc = cc/1.1;
       ccsave(i,1) = cc;
       end;
       if acc_rate(i,1) > 0.6
       cc = cc*1.1;
       ccsave(i,1) = cc;
       end;
   
    %Draw from lamda conditional on rest
          e = y - x*bdraw;
          dof=vdraw+1;
          chiv = chis_rnd(n,dof);   
          sige = 1/hdraw;
          vi = ((e.*e./sige) + in*vdraw)./chiv;
          lamdraw = (in./vi);
          pdraw = sqrt(lamdraw);   


    if i>s0
        %after discarding burnin, store all draws
        b_ = [b_ bdraw];
        h_ = [h_ hdraw];
        v_ = [v_ vdraw];
        vmean = vmean + vi;
    end
    
waitbar(i/s);         

end % end of sampling loop

close(hwait);

plot(b_');
title('bdraws');
pause;
tmp = (ones(ndraw,1)./h_');
plot(tmp);
title('sige draws');
pause;
plot(v_');
title('rdraws');
pause;
plot(acc_rate);
title('acceptance rate');
pause;

alldraws = [b_' h_' v_'];
resultt = momentg(alldraws);

% 95% HPDIs
% beta followed by h followed by r'
hpdis=zeros(k+2,2);
for ii=1:k+2
    hpdis(ii,1:2) = hpdi(alldraws(:,ii),.95);
end

% send back results structure
results.meth = 'ols_gv';
results.bdraw = b_';
results.hdraw = h_';
results.vmean = vmean/(ndraw-nomit);
results.rdraw = v_';
results.acc = acc_rate(nomit+1:end,1);
results.means=[resultt.pmean]';
results.stdevs=[resultt.pstd]';
results.nse=[resultt.nse1]';
results.hpd = hpdis;
results.pmean = b0;
results.pstd = diag(sqrt(capv0));
results.rmean = mean(v_');
results.nvar = k;
results.nobs = n;
results.ndraw = ndraw;
results.nomit = nomit;
results.y = y;
results.x = x;
results.v0 = v0;
results.nu = nu0;
results.d0 = d0;

function bounds = hpdi(adraw,hperc)
%This function takes a vector of MCMC draws and calculates
%a hperc-percent HPDI
ndraw=size(adraw,1);
botperc=round(hperc*ndraw);
topperc=round((1-hperc)*ndraw);
temp = sort(adraw,1);
bounds=[temp(topperc,1) temp(botperc,1)];

