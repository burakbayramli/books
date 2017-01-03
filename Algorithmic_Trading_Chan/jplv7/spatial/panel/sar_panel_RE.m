function results = sar_panel_RE(y,x,W,T,info)
% PURPOSE: computes spatial lag model estimates for spatial panels 
%          (N regions*T time periods) with spatial random effects (u) 
%          y = p*W*y + X*b + u + e, using sparse matrix algorithms
% Supply data sorted first by time and then by spatial units, so first region 1,
% region 2, et cetera, in the first year, then region 1, region 2, et
% cetera in the second year, and so on
% ---------------------------------------------------
%  USAGE: results = sar_panel_RE(y,x,W,T,info)
%  where:  y = dependent variable vector
%          x = independent variables matrix
%          W = spatial weights matrix (standardized)
%          T = number of points in time
%       info = an (optional) structure variable with input options:
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
%         results.meth  = 'sarsre'
%         results.beta  = bhat
%         results.rho   = rho (p above)
%         results.cov   = asymptotic variance-covariance matrix of the parameters b(eta),rho and teta
%         results.tstat = asymp t-stat (last entries are rho=spatial autoregressive coefficient and 
%                         teta=weight attached to cross-sectional variation in the data)
%         results.yhat  = [inv(y-p*W)]*[x*b+BLUP-correction term] (according to prediction formula)
%         results.resid = y-p*W*y-x*b
%         results.sige  = (y-p*W*y-x*b)'*(y-p*W*y-x*b)/nobs
%         results.rsqr  = rsquared
%         results.corr2 = goodness-of-fit between actual and fitted values
%         results.lik   = log likelihood
%         results.nobs  = # of observations
%         results.nvar  = # of explanatory variables in x 
%         results.iter  = # of iterations taken
%         results.rmax  = 1/max eigenvalue of W (or rmax if input)
%         results.rmin  = 1/min eigenvalue of W (or rmin if input)
%         results.lflag = lflag from input
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

time1 = 0; 
time2 = 0;
time3 = 0;
time4 = 0;

timet = clock; % start the clock for overall timing

W=sparse(W);

% if we have no options, invoke defaults
if nargin == 4
    info.lflag = 1;
    info.Nhes=500;
    fprintf(1,'default: pooled model without fixed effects \n');
end;

Nhes=500;

fields = fieldnames(info);
nf = length(fields);
if nf > 0
    for i=1:nf
        if strcmp(fields{i},'Nhes') Nhes = info.Nhes;
        end
    end
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

for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    Wy(t1:t2,1)=W*y(t1:t2,1);
end

% demeaning of the y and x variables, depending on (info.)model

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
    
ee=ones(T,1);
if (N < Nhes) info.lflag=0; else info.lflag=1; end
info.model=0;
t0 = clock;
par=[0.1; (x'*x)\x'*y];teta=0.1; %start values
iter=0;converge=1.0;criteria=1e-8;itermax=100;
options.Display='off';
options.MaxFunEvals=1000;
options.MaxIter=1000;
options.TolX=0.005;
options.TolFun=0.001;
while (converge>criteria & iter < itermax)
   tetaold=teta;
   teta=fminbnd('f_resar',0,1,options,par,y,Wy,x,meanny,meannwy,meannx,N,T); 
   eteta=ee-teta;
   yran=y-kron(eteta,meanny);
   xran=x-kron(eteta,meannx);
   results=sar_panel_FE(yran,xran,W,T,info); %routine elhorst, note info.model must be 0!!!
   par=[results.rho;results.beta];
   iter=iter+1;
   converge=abs(teta-tetaold);
end;
results.iter=iter;
results.meth='sarsre';
results.teta=teta;
p=results.rho;
bhat=results.beta;
sige=results.sige;
resid=results.resid;
time4 = etime(clock,t0);

% r-squared and corr-squared between actual and fitted values
yme=y-mean(y);
rsqr2=yme'*yme;
rsqr1 = resid'*resid;
results.rsqr=1.0-rsqr1/rsqr2; %rsquared

res=y-x*bhat;
meanres=zeros(N,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    meanres=meanres+res(t1:t2)/T;
end
blup=(1-teta^2)*meanres;

yhat=zeros(nobs,1);
yranhat=zeros(nobs,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    yranhat(t1:t2,1)=(speye(N) - p*W)\x(t1:t2,:)*bhat;
    yhat(t1:t2,1)=(speye(N) - p*W)\(x(t1:t2,:)*bhat+blup);
end

res1=y-mean(y);
res2=yranhat-mean(y);
rsq1=res1'*res2;
rsq2=res1'*res1;
rsq3=res2'*res2;
results.corr2=rsq1^2/(rsq2*rsq3); %corr2
results.yhat=yhat;

parm=[results.rho; results.beta; teta; sige];
results.lik=f2_resar(parm,y,Wy,x,meanny,meannwy,meannx,N,T,nvar,results.lndet);

% Determination variance-covariance matrix
if N <= Nhes % Analytically
t0 = clock;
B = speye(N) - p*W; 
BI = inv(B); WB = W*BI;
pterm = trace(WB*WB + WB'*WB);
xpx = zeros(nvar+3,nvar+3);               
% bhat,bhat
xpx(1:nvar,1:nvar) = (1/sige)*(xran'*xran);     
% bhat,rho
ysum=zeros(nvar,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    ysum=ysum+(1/sige)*xran(t1:t2,:)'*WB*xran(t1:t2,:)*bhat;
end
xpx(1:nvar,nvar+1) = ysum;
xpx(nvar+1,1:nvar) = xpx(1:nvar,nvar+1)'; 
% rho,rho
ysom=0;
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    ysom=ysom+(1/sige)*bhat'*xran(t1:t2,:)'*WB'*WB*xran(t1:t2,:)*bhat + pterm;
end
xpx(nvar+1,nvar+1) = ysom;
% rho,teta
xpx(nvar+1,nvar+2) = -trace(WB);  
xpx(nvar+2,nvar+1) = xpx(nvar+1,nvar+2);
% rho,sige
xpx(nvar+1,nvar+3) = (T/sige)*trace(WB);  
xpx(nvar+3,nvar+1) = xpx(nvar+1,nvar+3);
% teta,teta
xpx(nvar+2,nvar+2) = N*(1+1/teta^2);
% teta,sige
xpx(nvar+2,nvar+3) = -N/sige;
xpx(nvar+3,nvar+2) = xpx(nvar+2,nvar+3);
% sige, sige
xpx(nvar+3,nvar+3) = nobs/(2*sige*sige);    
xpxi = xpx\eye(size(xpx));
results.cov=xpxi(1:nvar+1,1:nvar+1);
tmp = diag(xpxi(1:nvar+2,1:nvar+2));
bvec = [results.beta
        results.rho
        results.teta];
tmp = bvec./(sqrt(tmp));
results.tstat = tmp;
time3 = etime(clock,t0);

else  % asymptotic t-stats using numerical hessian
t0 = clock;
dhessn=hessian('f2_resar',parm,y,Wy,x,meanny,meannwy,meannx,N,T,nvar,results.lndet); %elhorst
hessi = invpd(-dhessn);
results.cov=hessi(1:nvar+1,1:nvar+1);
tvar = abs(diag(hessi));
tmp = [results.beta
       results.rho
       results.teta];
results.tstat = tmp./sqrt(tvar(1:end-1,1));
time3 = etime(clock,t0);

end; % end of t-stat calculations

% return stuff
results.nobs  = nobs; 
results.nvar  = nvar;
results.time  = etime(clock,timet);
results.time3 = time3;
results.time4 = time4;
