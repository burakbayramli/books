function results = sar_panel_FE(y,x,W,T,info)
% PURPOSE: computes spatial lag model estimates for spatial panels 
%          (N regions*T time periods) with spatial fixed effects (u) 
%          and/or time period fixed effects (v)
%          y = p*W*y + X*b + u (optional) + v(optional) + e, using sparse matrix algorithms
% Supply data sorted first by time and then by spatial units, so first region 1,
% region 2, et cetera, in the first year, then region 1, region 2, et
% cetera in the second year, and so on
% sar_panel_FE computes y and x in deviation of the spatial and/or time means
% ---------------------------------------------------
%  USAGE: results = sar_panel_FE(y,x,W,T,info)
%  where:  y = dependent variable vector
%          x = independent variables matrix
%          W = spatial weights matrix (standardized)
%          T = number of points in time
%       info = an (optional) structure variable with input options:
%       info.model = 0 pooled model without fixed effects (default, x may contain an intercept)
%                  = 1 spatial fixed effects (x may not contain an intercept)
%                  = 2 time period fixed effects (x may not contain an intercept)
%                  = 3 spatial and time period fixed effects (x may not contain an intercept)
%       info.fe    = report fixed effects and their t-values in prt_sp (default=0=not reported; info.fe=1=report) 
%       info.Nhes  = N =< Nhes asymptotic variance matrix is computed using analytical formulas,
%                    N > Nhes asymptotic variance matrix is computed using numerical formulas
%                    (Default NHes=500)
%       info.rmin  = (optional) minimum value of rho to use in search  
%       info.rmax  = (optional) maximum value of rho to use in search    
%       info.convg = (optional) convergence criterion (default = 1e-8)
%       info.maxit = (optional) maximum # of iterations (default = 500)
%       info.lflag = 0 for full lndet computation (default = 1, fastest)
%                  = 1 for MC lndet approximation (fast for very large problems)
%                  = 2 for Spline lndet approximation (medium speed)
%       info.order = order to use with info.lflag = 1 option (default = 50)
%       info.iter  = iterations to use with info.lflag = 1 option (default = 30)  
%       info.lndet = a matrix returned by sar containing log-determinant information to save time
% ---------------------------------------------------
%  RETURNS: a structure
%         results.meth  = 'psar' if infomodel=0
%                       = 'sarsfe' if info.model=1
%                       = 'sartfe' if info.model=2
%                       = 'sarstfe' if info.model=3
%         results.beta  = bhat
%         results.rho   = rho (p above)
%         results.cov   = asymptotic variance-covariance matrix of the parameters b(eta) and rho
%         results.tstat = asymp t-stat (last entry is rho=spatial autoregressive coefficient)
%         results.yhat  = [inv(y-p*W)]*[x*b+fixed effects] (according to prediction formula)
%         results.resid = y-p*W*y-x*b
%         results.sige  = (y-p*W*y-x*b)'*(y-p*W*y-x*b)/n
%         results.rsqr  = rsquared
%         results.corr2 = goodness-of-fit between actual and fitted values
%         results.sfe   = spatial fixed effects (if info.model=1 or 3)
%         results.tfe   = time period fixed effects (if info.model=2 or 3)
%         results.tsfe  = t-values spatial fixed effects (if info.model=1 or 3)
%         results.ttfe  = t-values time period fixed effects (if info.model=2 or 3)
%         results.con   = intercept 
%         results.con   = t-value intercept
%         results.lik   = log likelihood
%         results.nobs  = # of observations
%         results.nvar  = # of explanatory variables in x 
%         results.tnvar = nvar + W*y + # fixed effects
%         results.iter  = # of iterations taken
%         results.rmax  = 1/max eigenvalue of W (or rmax if input)
%         results.rmin  = 1/min eigenvalue of W (or rmin if input)
%         results.lflag = lflag from input
%         results.fe    = fe from input
%         results.liter = info.iter option from input
%         results.order = info.order option from input
%         results.limit = matrix of [rho lower95,logdet approx, upper95] intervals
%                         for the case of lflag = 1
%         results.time1 = time for log determinant calcluation
%         results.time2 = time for eigenvalue calculation
%         results.time3 = time for hessian or information matrix calculation
%         results.time4 = time for optimization
%         results.time  = total time taken      
%         results.lndet = a matrix containing log-determinant information
%                          (for use in later function calls to save time)
% --------------------------------------------------
%  NOTES: if you use lflag = 1 or 2, info.rmin will be set = -1 
%                                    info.rmax will be set = 1
%         For number of spatial units < 500 you should use lflag = 0 to get
%         exact results, 
%         Fixed effects and their t-values are calculated as the deviation
%         from the mean intercept
% ---------------------------------------------------
%
% Updated by: J.Paul Elhorst summer 2008
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCES: 
% Elhorst JP (2003) Specification and Estimation of Spatial Panel Data Models,
% International Regional Science Review 26: 244-268.
% Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.

% This function is partly based on James. P LeSage's function SAR

time1 = 0; 
time2 = 0;
time3 = 0;
time4 = 0;

timet = clock; % start the clock for overall timing

W=sparse(W);

% if we have no options, invoke defaults
if nargin == 4
    info.lflag = 1;
    info.model=0;
    info.Nhes=500;
    fprintf(1,'default: pooled model without fixed effects \n');
end;

fe=0;
model=0;
Nhes=500;

fields = fieldnames(info);
nf = length(fields);
if nf > 0
    for i=1:nf
        if strcmp(fields{i},'model') model = info.model;
        elseif strcmp(fields{i},'fe') fe = info.fe;
        elseif strcmp(fields{i},'Nhes') Nhes = info.Nhes;
        end
    end
end
if model==0
    results.meth='psar';
elseif model==1
    results.meth='sarsfe';
elseif model==2
    results.meth='sartfe';
elseif model==3
    results.meth='sarstfe';
else
    error('sar_panel: wrong input number of info.model');
end

% check size of user inputs for comformability
[nobs nvar] = size(x);
[N Ncol] = size(W);
if N ~= Ncol
error('sar: wrong size weight matrix W');
elseif N ~= nobs/T
error('sar: wrong size weight matrix W or matrix x');
end;
[nchk junk] = size(y);
if nchk ~= nobs
error('sar: wrong size vector y or matrix x');
end;

if (fe==1 & model==0 ) error('info.fe=1, but cannot compute fixed effects if info.model is set to 0 or not specified'); end

% parse input options
%[rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,miter,options] = sar_parse(info); % function of LeSage
[rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,miter,options,ndraw,sflag,p,cflag] = sar_parse(info);

% compute eigenvalues or limits
[rmin,rmax,time2] = sar_eigs(eflag,W,rmin,rmax,N); % function of LeSage

% do log-det calculations
[detval,time1] = sar_lndet(ldetflag,W,rmin,rmax,detval,order,miter); % function of LeSage

for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    Wy(t1:t2,1)=W*y(t1:t2,1);
end

% demeaning of the y and x variables, depending on (info.)model

if (model==1 | model==3);
meanny=zeros(N,1);
meannwy=zeros(N,1);
meannx=zeros(N,nvar);
for i=1:N
    ym=zeros(T,1);
    wym=zeros(T,1);
    xm=zeros(T,nvar);
    for t=1:T
        ym(t)=y(i+(t-1)*N,1);
        wym(t)=Wy(i+(t-1)*N,1);
        xm(t,:)=x(i+(t-1)*N,:);
    end
    meanny(i)=mean(ym);
    meannwy(i)=mean(wym);
    meannx(i,:)=mean(xm);
end
clear ym wym xm;
end % if statement

if ( model==2 | model==3)
meanty=zeros(T,1);
meantwy=zeros(T,1);
meantx=zeros(T,nvar);
for i=1:T
    t1=1+(i-1)*N;t2=i*N;
    ym=y([t1:t2],1);
    wym=Wy([t1:t2],1);
    xm=x([t1:t2],:);
    meanty(i)=mean(ym);
    meantwy(i)=mean(wym);
    meantx(i,:)=mean(xm);
end
clear ym wym xm;
end % if statement
    
en=ones(T,1);
et=ones(N,1);
ent=ones(nobs,1);

if model==1
    ywith=y-kron(en,meanny);
    wywith=Wy-kron(en,meannwy);
    xwith=x-kron(en,meannx);
elseif model==2
    ywith=y-kron(meanty,et);
    wywith=Wy-kron(meantwy,et);
    xwith=x-kron(meantx,et);
elseif model==3
    ywith=y-kron(en,meanny)-kron(meanty,et)+kron(ent,mean(y));
    wywith=Wy-kron(en,meannwy)-kron(meantwy,et)+kron(ent,mean(Wy));
    xwith=x-kron(en,meannx)-kron(meantx,et)+kron(ent,mean(x));
else
    ywith=y;
    wywith=Wy;
    xwith=x;
end % if statement

% step 1) do regressions
t0 = clock;
          AI = xwith'*xwith;
          b0 = AI\(xwith'*ywith);
          bd = AI\(xwith'*wywith);
          e0 = ywith - xwith*b0;
          ed = wywith - xwith*bd;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;

% step 2) maximize concentrated likelihood function;
	options = optimset('fminbnd');
    [p,liktmp,exitflag,output] = fminbnd('f_sarpanel',rmin,rmax,options,detval,epe0,eped,epe0d,N,T);
   
time4 = etime(clock,t0);

if exitflag == 0
fprintf(1,'sar: convergence concentrated likelihood function not obtained in %4d iterations \n',output.iterations);
end;
results.iter = 1;

% step 3) find b,sige maximum likelihood estimates
results.beta = b0 - p*bd; 
results.rho = p; 
bhat = results.beta;
results.sige = (1/nobs)*(e0-p*ed)'*(e0-p*ed); 
sige = results.sige;

% step 4) find fixed effects and their t-values
if model==1
    intercept=mean(y)-mean(Wy)*results.rho-mean(x)*results.beta;
    results.con=intercept;
    results.sfe=meanny-meannwy*results.rho-meannx*results.beta-kron(et,intercept);
    xhat=x*results.beta+kron(en,results.sfe)+kron(ent,intercept);
    results.tsfe=results.sfe./sqrt(sige/T*ones(N,1)+diag(sige*meannx*(xwith'*xwith)*meannx'));
    results.tcon=results.con/sqrt(sige/nobs+sige*mean(x)*(xwith'*xwith)*mean(x)');
    tnvar=nvar+N; 
elseif model==2
    intercept=mean(y)-mean(Wy)*results.rho-mean(x)*results.beta;
    results.con=intercept;
    results.tfe=meanty-meantwy*results.rho-meantx*results.beta-kron(en,intercept); 
    xhat=x*results.beta+kron(results.tfe,et)+kron(ent,intercept);
    results.ttfe=results.tfe./sqrt(sige/N*ones(T,1)+diag(sige*meantx*(xwith'*xwith)*meantx'));
    results.tcon=results.con/sqrt(sige/nobs+sige*mean(x)*(xwith'*xwith)*mean(x)');
    tnvar=nvar+T;
elseif model==3
    intercept=mean(y)-mean(Wy)*results.rho-mean(x)*results.beta; 
    results.con=intercept;
    results.sfe=meanny-meannwy*results.rho-meannx*results.beta-kron(et,intercept);
    results.tfe=meanty-meantwy*results.rho-meantx*results.beta-kron(en,intercept);
    results.tsfe=results.sfe./sqrt(sige/T*ones(N,1)+diag(sige*meannx*(xwith'*xwith)*meannx'));
    results.ttfe=results.tfe./sqrt(sige/N*ones(T,1)+diag(sige*meantx*(xwith'*xwith)*meantx'));
    results.tcon=results.con/sqrt(sige/nobs+sige*mean(x)*(xwith'*xwith)*mean(x)');
    xhat=x*results.beta+kron(en,results.sfe)+kron(results.tfe,et)+kron(ent,intercept);
    tnvar=nvar+T;
else
    xhat=x*results.beta;
    tnvar=nvar;
end    

% r-squared and corr-squared between actual and fitted values
results.tnvar=tnvar;
results.resid = y - p*Wy - xhat; 
yme=y-mean(y);
rsqr2=yme'*yme;
rsqr1 = results.resid'*results.resid;
results.rsqr=1.0-rsqr1/rsqr2; %rsquared

yhat=zeros(nobs,1);
ywithhat=zeros(nobs,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    ywithhat(t1:t2,1)=(speye(N) - p*W)\xwith(t1:t2,:)*results.beta;
    yhat(t1:t2,1)=(speye(N) - p*W)\xhat(t1:t2,1);
end
res1=ywith-mean(ywith);
res2=ywithhat-mean(ywith);
rsq1=res1'*res2;
rsq2=res1'*res1;
rsq3=res2'*res2;
results.corr2=rsq1^2/(rsq2*rsq3); %corr2
results.yhat=yhat;

parm = [results.beta
        results.rho
        results.sige];

results.lik = f2_sarpanel(parm,ywith,xwith,W,detval,T); %Elhorst

% Determination variance-covariance matrix
if N <= Nhes % Analytically
t0 = clock;
B = speye(N) - p*W; 
BI = inv(B); WB = W*BI;
pterm = trace(WB*WB + WB'*WB);
xpx = zeros(nvar+2,nvar+2);               
% bhat,bhat
xpx(1:nvar,1:nvar) = (1/sige)*(xwith'*xwith);     
% bhat,rho
ysum=zeros(nvar,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    ysum=ysum+(1/sige)*xwith(t1:t2,:)'*WB*xwith(t1:t2,:)*bhat;
end
xpx(1:nvar,nvar+1) = ysum;
xpx(nvar+1,1:nvar) = xpx(1:nvar,nvar+1)'; 
% rho,rho
ysom=0;
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    ysom=ysom+(1/sige)*bhat'*xwith(t1:t2,:)'*WB'*WB*xwith(t1:t2,:)*bhat + pterm;
end
xpx(nvar+1,nvar+1) = ysom;
% sige, sige
xpx(nvar+2,nvar+2) = nobs/(2*sige*sige);     
% rho,sige
xpx(nvar+1,nvar+2) = (T/sige)*trace(WB);  
xpx(nvar+2,nvar+1) = xpx(nvar+1,nvar+2);
xpxi = xpx\eye(size(xpx));
results.cov=xpxi(1:nvar+1,1:nvar+1);
tmp = diag(xpxi(1:nvar+1,1:nvar+1));
bvec = [results.beta
        results.rho];
tmp = bvec./(sqrt(tmp));
results.tstat = tmp;
time3 = etime(clock,t0);

else  % asymptotic t-stats using numerical hessian
t0 = clock;
dhessn = hessian('f2_sarpanel',parm,ywith,xwith,W,detval,T); %Elhorst
hessi = invpd(-dhessn);
results.cov=hessi(1:nvar+1,1:nvar+1);
tvar = abs(diag(hessi));
tmp = [results.beta
       results.rho];
results.tstat = tmp./sqrt(tvar(1:end-1,1));
time3 = etime(clock,t0);

end; % end of t-stat calculations

% return stuff
results.nobs  = nobs; 
results.nvar  = nvar;
results.rmax  = rmax;      
results.rmin  = rmin;
results.lflag = ldetflag;
results.order = order;
results.miter = miter;
results.fe    = fe;
results.time  = etime(clock,timet);
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.time4 = time4;
results.lndet = detval;
results.N     = N;
results.T     = T;
results.model = model;


function [rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,iter,options,ndraw,sflag,p,cflag] = sar_parse(info)
% PURPOSE: parses input arguments for sar model
% ---------------------------------------------------
%  USAGE: [rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,iter,options] = sar_parse(info)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% set defaults
options = zeros(1,18); % optimization options for fminbnd
options(1) = 0; 
options(2) = 1.e-6; 
options(14) = 500;

eflag = 0;     % default to not computing eigenvalues
ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
order = 50;    % there are parameters used by the MC det approx
iter = 30;     % defaults based on Pace and Barry recommendation
rmin = -1;     % use -1,1 rho interval as default
rmax = 1;
detval = 0;    % just a flag
convg = 0.0001;
maxit = 500;
ndraw = 1000;
sflag = 0;
p = 0;
cflag = 0;

fields = fieldnames(info);
nf = length(fields);
if nf > 0
    
 for i=1:nf
    if strcmp(fields{i},'rmin')
        rmin = info.rmin;  eflag = 0;
    elseif strcmp(fields{i},'rmax')
        rmax = info.rmax; eflag = 0;
    elseif strcmp(fields{i},'p')
        p = info.p;
    elseif strcmp(fields{i},'cflag')
        cflag = info.cflag;
    elseif strcmp(fields{i},'convg')
        options(2) = info.convg;
    elseif strcmp(fields{i},'maxit')
        options(14) = info.maxit;  
    elseif strcmp(fields{i},'lndet')
    detval = info.lndet;
    ldetflag = -1;
    eflag = 0;
    rmin = detval(1,1);
    nr = length(detval);
    rmax = detval(nr,1);
    elseif strcmp(fields{i},'lflag')
        tst = info.lflag;
        if tst == 0,
        ldetflag = 0; % compute full lndet, no approximation
        elseif tst == 1,
        ldetflag = 1; % use Pace-Barry approximation
        elseif tst == 2,
        ldetflag = 2; % use spline interpolation approximation
        else
        error('sar: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = info.order;  
    elseif strcmp(fields{i},'eig')
        eflag = info.eig;  
    elseif strcmp(fields{i},'iter')
        iter = info.iter; 
     elseif strcmp(fields{i},'ndraw')
        ndraw = info.ndraw; 
     elseif strcmp(fields{i},'sflag')
        sflag = info.sflag; 
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


if eflag == 1 % do eigenvalue calculations
t0 = clock;
opt.tol = 1e-3; opt.disp = 0;
lambda = eigs(sparse(W),speye(n),1,'SR',opt);  
rmin = real(1/lambda);   
rmax = 1.0;
time2 = etime(clock,t0);
else % use rmin,rmax arguments from input or defaults -1,1
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
            error('sar: wrong lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('sar: wrong sized lndet input argument');
        elseif n1 == 1
            error('sar: wrong sized lndet input argument');
        end;          
end;



function H = hessian(f,x,varargin)
% PURPOSE: Computes finite difference Hessian
% -------------------------------------------------------
% Usage:  H = hessian(func,x,varargin)
% Where: func = function name, fval = func(x,varargin)
%           x = vector of parameters (n x 1)
%    varargin = optional arguments passed to the function
% -------------------------------------------------------
% RETURNS:
%           H = finite differnce hessian
% -------------------------------------------------------

% Code from:
% COMPECON toolbox [www4.ncsu.edu/~pfackler]
% documentation modified to fit the format of the Ecoometrics Toolbox
% by James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

eps = 1e-6;

n = size(x,1);
fx = feval(f,x,varargin{:});
 
% Compute the stepsize (h)
h = eps.^(1/3)*max(abs(x),1e-2);
xh = x+h;
h = xh-x;    
ee = sparse(1:n,1:n,h,n,n);
 
% Compute forward step 
g = zeros(n,1);
for i=1:n
  g(i) = feval(f,x+ee(:,i),varargin{:});
end
   
H=h*h';
% Compute "double" forward step 
for i=1:n
for j=i:n
  H(i,j) = (feval(f,x+ee(:,i)+ee(:,j),varargin{:})-g(i)-g(j)+fx)/H(i,j);
  H(j,i) = H(i,j);
end
end


