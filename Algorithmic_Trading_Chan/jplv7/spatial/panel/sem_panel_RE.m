function results = sem_panel_RE(y,x,W,T,info)
% PURPOSE: computes spatial error model estimates for spatial panels 
%          (N regions*T time periods) with spatial random effects (u) 
%          y = XB + u + s,  s = p*W*s + e
% Supply data sorted first by time and then by spatial units, so first region 1,
% region 2, et cetera, in the first year, then region 1, region 2, et
% cetera in the second year, and so on
% ---------------------------------------------------
%  USAGE: results = sem_panel_RE(y,x,W,T,info)
%  where:  y = dependent variable vector
%          x = independent variables matrix
%          W = spatial weights matrix
%          T = number of points in time
%       info = an (optional) structure variable with input options:
%       info.Nhes  = N =< Nhes asymptotic variance matrix is computed using analytical formulas,
%                              and W may be row-normalized 
%                    N > Nhes asymptotic variance matrix is computed using numerical formulas,
%                             W should be in raw-form, symmetric and not row-normalized
%                    (Default NHes=500)
% ---------------------------------------------------
%  RETURNS: a structure
%         results.meth  = 'semsre'
%         results.beta  = bhat
%         results.rho   = rho (p above)
%         results.cov   = asymptotic variance-covariance matrix of the parameters b(eta),rho and teta
%         results.tstat = asymp t-stat (last entries are rho=spatial autoregressive coefficient and 
%                         teta=weight attached to cross-sectional variation in the data)
%         results.yhat  = x*b+BLUP-correction term (according to prediction formula)
%         results.resid = y-x*b
%         results.sige  = e'(I-p*W)'*(I-p*W)*e/nobs
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
%         results.time2 = time for eigenvalue calculation
%         results.time3 = time for hessian or information matrix calculation
%         results.time4 = time for optimization
%         results.time  = total time taken      
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
    info.Nhes=500;
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
error('sem: wrong size weight matrix W');
elseif N ~= nobs/T
error('sem: wrong size weight matrix W or matrix x');
end;
[nchk junk] = size(y);
if nchk ~= nobs
error('sem: wrong size vector y or matrix x');
end;

if ( N>Nhes) options.disp=0;
lambda=eigs(W,1,'LA',options);
W=W/lambda;
clear lambda;
end

wy=zeros(N*T,1);
wx=zeros(N*T,nvar);
for t=1:T
   ti=1+(t-1)*N;tj=t*N;
   wy(ti:tj)=W*y(ti:tj);
   wx(ti:tj,:)=W*x(ti:tj,:);
end
teta=1;rho=0.1;iter=0;converge=1.0;criteria=1e-8;itermax=100;
options.Display='off';
options.MaxFunEvals=1000;
options.MaxIter=1000;
options.TolX=0.005;
options.TolFun=0.001;

meany=zeros(N,1);
meanx=zeros(N,nvar);
for i=1:N
    ym=zeros(T,1);
    xm=zeros(T,nvar);
    for t=1:T
        ym(t)=y(i+(t-1)*N,1);
        xm(t,:)=x(i+(t-1)*N,:);
    end
    meany(i,1)=mean(ym);
    meanx(i,:)=mean(xm);
end
clear ym xm;

t0 = clock;
[V D]=eig(full(W));
lambda=diag(D);
clear D;
rmin=1/min(lambda);
rmax=1/max(lambda);
wmeany=W*meany;
wmeanx=W*meanx;
vmeany=V'*meany;
vmeanx=V'*meanx;
time2 = etime(clock,t0);

ee=ones(T,1);
eigw=zeros(N,1);
meanpy=zeros(N,1);
meanpx=zeros(N,nvar);
t0 = clock;

while ( converge>criteria & iter < itermax)
   iter=iter+1;
   tetaold=teta;
   rhoold=rho;
   b=[rho;teta];
   for i=1:N
      eigw(i)=(T*teta^2+1/(1-rho*lambda(i))^2)^(-0.5);
      meanpy(i,1)=eigw(i)*vmeany(i,1)-(meany(i,1)-rho*wmeany(i,1));
      meanpx(i,:)=eigw(i)*vmeanx(i,:)-(meanx(i,:)-rho*wmeanx(i,:));
   end
   yran=y-rho*wy+kron(ee,meanpy);
   xran=x-rho*wx+kron(ee,meanpx);
   results=ols(yran,xran);
   beta=results.beta;
   btemp=fminsearch('f_respat',b,options,beta,y,x,wy,wx,lambda,meany,meanx,wmeany,wmeanx,vmeany,vmeanx,N,T,nvar); %elhorst
   rho=btemp(1);
   teta=btemp(2);
   converge=abs(rho-rhoold)+abs(teta-tetaold);
end
results.iter=iter;
results.meth='semsre';
resid=results.resid;
res2=resid'*resid;
sige=res2/nobs;
results.sige=sige;
results.teta=teta^2;
p=rho;
results.rho=p;
bhat=results.beta;
time4 = etime(clock,t0);

% r-squared and corr-squared between actual and fitted values
yme=y-mean(y);
rsqr2=yme'*yme;
rsqr1 = resid'*resid;
results.rsqr=1.0-rsqr1/rsqr2; %rsquared

res=y-x*bhat;
sumres=zeros(N,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    sumres=sumres+res(t1:t2);
end
eigw=zeros(N,1);
for i=1:N;
    eigw(i)=(T*teta^2+1/(1-rho*lambda(i))^2)^(-1);
end
blup=teta^2*V*diag(eigw)*V'*sumres;

yhat=zeros(nobs,1);
yranhat=zeros(nobs,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    yranhat(t1:t2,1)=x(t1:t2,:)*bhat;
    yhat(t1:t2,1)=x(t1:t2,:)*bhat+blup;
end

res1=y-mean(y);
res2=yranhat-mean(y);
rsq1=res1'*res2;
rsq2=res1'*res1;
rsq3=res2'*res2;
results.corr2=rsq1^2/(rsq2*rsq3); %corr2
results.yhat=yhat;
results.lik=f2_respat([p;teta;sige],y,x,wy,wx,lambda,bhat,meany,meanx,wmeany,wmeanx,vmeany,vmeanx,N,T,nvar); %elhorst

% Determination variance-covariance matrix
if N <= Nhes % Analytically
t0 = clock;
B = speye(N) - p*W; 
BI = inv(B'*B); 
GAM=(W'*B+B'*W)*BI;
VI=V*diag(eigw)*V';
SIG=VI*BI;
xpx = zeros(nvar+3,nvar+3);               
% bhat,bhat
xpx(1:nvar,1:nvar) = (1/sige)*(xran'*xran);     
% rho,rho
xpx(nvar+1,nvar+1) = (T-1)/2*trace(GAM*GAM)+1/2*trace(GAM*SIG*GAM*SIG);
% rho,teta
xpx(nvar+1,nvar+2) = T/(2*sige)*trace(SIG*GAM*VI);  
xpx(nvar+2,nvar+1) = xpx(nvar+1,nvar+2);
% rho,sige
xpx(nvar+1,nvar+3) = (T-1)/(2*sige)*trace(GAM)+1/(2*sige)*trace(SIG*GAM*SIG);  
xpx(nvar+3,nvar+1) = xpx(nvar+1,nvar+3);
% teta,teta
xpx(nvar+2,nvar+2) = T^2/(2*sige*sige)*trace(VI*VI);
% teta,sige
xpx(nvar+2,nvar+3) = T/(2*sige*sige)*trace(SIG*VI);
xpx(nvar+3,nvar+2) = xpx(nvar+2,nvar+3);
% sige, sige
xpx(nvar+3,nvar+3) = 1/(2*sige*sige)*((T-1)*N+trace(GAM*GAM));    
xpxi = xpx\eye(size(xpx));
results.cov=xpxi(1:nvar+1,1:nvar+1);
tmp = diag(xpxi(1:nvar+2,1:nvar+2));
% correction t-value teta
sigmau=results.teta*sige;
tmp(nvar+2)=results.teta^2*(xpxi(nvar+2,nvar+2)/sigmau^2+xpxi(nvar+3,nvar+3)/sige^2-2*xpxi(nvar+2,nvar+3)/(sigmau*sige));

bvec = [results.beta
        results.rho
        results.teta];
tmp = bvec./(sqrt(tmp));
results.tstat = tmp;
time3 = etime(clock,t0);

else  % asymptotic t-stats using numerical hessian
t0 = clock;
hessn=zeros(nvar+3,nvar+3);
hessn(1:nvar,1:nvar)=(1/sige)*(xran'*xran);
hessn(nvar+1:nvar+3,nvar+1:nvar+3)=hessian('f2_respat',[rho;teta;sige],y,x,wy,wx,lambda,bhat,meany,meanx,wmeany,wmeanx,vmeany,vmeanx,N,T,nvar); %elhorst

if hessn(nvar+3,nvar+3) == 0
 hessn(nvar+3,nvar+3) = 1/sige;  % this is a hack for very large models that 
end;                             % should not affect inference in these cases

hessi = invpd(-hessn);
results.cov=hessi(1:nvar+1,1:nvar+1);
tvar = abs(diag(hessi));
bout=[results.beta;results.rho;results.teta];
results.tstat = bout./sqrt(tvar(1:end-1,1));

time3 = etime(clock,t0);

end; % end of t-stat calculations

% return stuff
results.nobs  = nobs; 
results.nvar  = nvar;
results.time  = etime(clock,timet);
results.time2 = time2;
results.time3 = time3;
results.time4 = time4;
results.rmin  = rmin;
results.rmax  = rmax;