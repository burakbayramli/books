function results = sarregime_panel(y,x,d,W,T,info)
% PURPOSE: computes two-regimes spatial lag model for spatial panels 
%          (N regions*T time periods) with spatial fixed effects (u) and/or
%          time period fixed effects (v)
%          y = p1*W*y + p2*W*y + X*b + u (optional) + v (optional) + e, 
%          using a binary variable d identifying the regimes
% Supply data sorted first by time and then by spatial units, so first region 1,
% region 2, et cetera, in the first year, then region 1, region 2, et
% cetera in the second year, and so on
% sarregime_panel computes y and x in deviation of the spatial and/or time means
% ---------------------------------------------------
%  USAGE: results = sarregime_panel(y,x,d,W,T,info)
%  where:  y = dependent variable vector
%          x = independent variables matrix
%          d = 0-1 variable identying to which regime each observations belongs
%          W = spatial weights matrix (standardized)
%          T = number of points in time
%       info = an (optional) structure variable with input options:
%       info.model = 0 pooled model without fixed effects (default, x may contain an intercept)
%                  = 1 spatial fixed effects (x may not contain an intercept)
%                  = 2 time period fixed effects (x may not contain an intercept)
%                  = 3 spatial and time period fixed effects (x may not contain an intercept)
% ---------------------------------------------------
%  RETURNS: a structure
%         results.meth  = 'sarreg' if infomodel=0
%                       = 'sarregsfe' if info.model=1
%                       = 'sarregtfe' if info.model=2
%                       = 'sarregstfe' if info.model=3
%         results.beta  = bhat
%         results.rho   = rho 1 and rho 2(p1 and p2 above)
%         results.tstat = asymp t-stat (last entries are rho1 and rho2=spatial autoregressive coefficients)
%         results.dif   = rho1-rho2
%         results.tdif  = t-value of rho1-rho2
%         results.cov   = asymptotic variance-covariance matrix of the parameters b(eta) and rhos
%         results.yhat  = yhat = [inv(I-p1*D*W-p2(I-D)*W2)]*[x*b+fixed effects] (according to prediction formula) 
%         results.resid = y-p1*D*W*y-p2*(I-D)*W*y-x*b
%         results.sige  = residuals'*residuals/nobs
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
%         results.tnvar = nvar + d*W*y + (I-D)*W*y + # fixed effects
%         results.iter  = # of iterations taken
%         results.rmax  = 1/max eigenvalue of W
%         results.rmin  = 1/min eigenvalue of W
%         results.time  = total time taken      
% --------------------------------------------------
%  NOTE:  Fixed effects and their t-values are calculated as the deviation
%         from the mean intercept
% ---------------------------------------------------
%
% written by: J.Paul Elhorst 2/2007
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCES: 
% Elhorst J.P., Fréret S. (2009) Evidence of political yardstick competition in France 
% using a two-regime spatial Durbin model with fixed effects. 
% Journal of Regional Science. Forthcoming.


timet = clock; % start the clock for overall timing

% if we have no options, invoke defaults
if nargin == 5
    info.model=0;
    fprintf(1,'default: pooled model without fixed effects \n');
end;

fields = fieldnames(info);
nf = length(fields);
if nf > 0
    for i=1:nf
        if strcmp(fields{i},'model') model = info.model;
        elseif strcmp(fields{i},'fe') fe = info.fe;
        end
    end
end
if model==0
    results.meth='sarreg';
elseif model==1
    results.meth='sarregsfe';
elseif model==2
    results.meth='sarregtfe';
elseif model==3
    results.meth='sarregstfe';
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

lambda=eig(W); %eigenvalues
rmin=min(lambda);
rmax=max(lambda);

% demeaning of the y and x variables, depending on (info.)model

if (model==1 | model==3);
meanny=zeros(N,1);
meannx=zeros(N,nvar);
for i=1:N
    ym=zeros(T,1);
    xm=zeros(T,nvar);
    for t=1:T
        ym(t)=y(i+(t-1)*N,1);
        xm(t,:)=x(i+(t-1)*N,:);
    end
    meanny(i)=mean(ym);
    meannx(i,:)=mean(xm);
end
clear ym xm;
end % if statement

if ( model==2 | model==3)
meanty=zeros(T,1);
meantx=zeros(T,nvar);
for i=1:T
    t1=1+(i-1)*N;t2=i*N;
    ym=y([t1:t2],1);
    xm=x([t1:t2],:);
    meanty(i)=mean(ym);
    meantx(i,:)=mean(xm);
end
clear ym xm;
end % if statement
    
en=ones(T,1);
et=ones(N,1);
ent=ones(nobs,1);

if model==1
    ywith=y-kron(en,meanny);
    xwith=x-kron(en,meannx);
elseif model==2
    ywith=y-kron(meanty,et);
    xwith=x-kron(meantx,et);
elseif model==3
    ywith=y-kron(en,meanny)-kron(meanty,et)+kron(ent,mean(y));
    xwith=x-kron(en,meannx)-kron(meantx,et)+kron(ent,mean(x));
else
    ywith=y;
    xwith=x;
end % if statement
%
% Two regimes
%
index=d;
Wy1=zeros(nobs,1);
Wy2=zeros(nobs,1);
wywith1=zeros(nobs,1);
wywith2=zeros(nobs,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    W1=W;
    W2=W;
    for i=1:N
        if (index((t-1)*N+i,1)==0) W2(i,1:N)=zeros(1,N);
        else W1(i,1:N)=zeros(1,N);
        end
    end
    Wy1(t1:t2,1) = W1*y(t1:t2,1);
    Wy2(t1:t2,1) = W2*y(t1:t2,1);
    wywith1(t1:t2,1) = W1*ywith(t1:t2,1);
    wywith2(t1:t2,1) = W2*ywith(t1:t2,1);
end

% step 1) do regressions
% step 2) maximize concentrated likelihood function;
AI = xwith'*xwith;
b0 = AI\(xwith'*ywith);
bd1 = AI\(xwith'*wywith1);
bd2 = AI\(xwith'*wywith2);
e0 = ywith - xwith*b0;
ed1 = wywith1 - xwith*bd1;
ed2 = wywith2 - xwith*bd2;
options = optimset('fminbnd');
rhos=[0.1;0.1];

[rhos,liktmp,exitflag,output]= fminsearch('f_sar2_panel',rhos,options,index,W,e0,ed1,ed2,N,T);

if exitflag == 0
fprintf(1,'sar: convergence not obtained in %4d iterations \n',output.iterations);
end;
results.iter = output.iterations;

% step 3) find b and sige maximum likelihood estimates

rho1=rhos(1);
rho2=rhos(2);
beta = b0 - rho1*bd1- rho2*bd2; 
sige = (1/nobs)*(e0-rho1*ed1-rho2*ed2)'*(e0-rho1*ed1-rho2*ed2); 
results.rho = rhos; 
results.beta = beta; 
results.sige=sige;

% step 4) find intercept and fixed effects

if model==1
    resh=y-rho1*Wy1-rho2*Wy2-x*beta;
    results.con=mean(resh);
    meanresn=zeros(N,1);
    for i=1:N
        rest=zeros(T,1);
        for t=1:T
            rest(t)=resh(i+(t-1)*N,1);
        end
        meanresn(i)=mean(rest);
    end
    clear rest;
    results.sfe=meanresn-kron(et,results.con);
    xhat=x*beta+kron(en,results.sfe)+kron(ent,results.con);
    tnvar=nvar+2+N; 
elseif model==2
    resh=y-rho1*Wy1-rho2*Wy2-x*beta;
    results.con=mean(resh);
    meanrest=zeros(T,1);
    for t=1:T
        t1=1+(t-1)*N;t2=t*N;
        resn=resh(t1:t2,1);
        meanrest(t)=mean(resn);
    end
    clear resn;
    results.tfe=meanrest-kron(en,results.con);
    xhat=x*beta+kron(results.tfe,et)+kron(ent,results.con);
    tnvar=nvar+2+T;
elseif model==3
    resh=y-rho1*Wy1-rho2*Wy2-x*beta;
    results.con=mean(resh);
    meanresn=zeros(N,1);
    for i=1:N
        rest=zeros(T,1);
        for t=1:T
            rest(t)=resh(i+(t-1)*N,1);
        end
        meanresn(i)=mean(rest);
    end
    clear rest;
    meanrest=zeros(T,1);
    for t=1:T
        t1=1+(t-1)*N;t2=t*N;
        resn=resh(t1:t2,1);
        meanrest(t)=mean(resn);
    end
    clear resn;
    results.sfe=meanresn-kron(et,results.con);
    results.tfe=meanrest-kron(en,results.con);
    xhat=x*beta+kron(en,results.sfe)+kron(results.tfe,et)+kron(ent,results.con);
    tnvar=nvar+1+N+T;
else
    xhat=x*results.beta;
    tnvar=nvar+2; % +2 due to two spatially lagged dependent variables
end    

yhat=zeros(nobs,1);

for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    W1=W;
    W2=W;
    for i=1:N
        if (index((t-1)*N+i,1)==0) W2(i,1:N)=zeros(1,N);
        else W1(i,1:N)=zeros(1,N);
        end
    end
    yhat(t1:t2,1)=(eye(N) - rho1*W1-rho2*W2)\xhat(t1:t2,1);
end

results.yhat = yhat;
resid=ywith-wywith1*rho1-wywith2*rho2-xwith*beta;
results.resid = resid; 

% R-squared
yme=y-mean(y);
rsqr2=yme'*yme;
rsqr1 = resid'*resid;
results.rsqr=1.0-rsqr1/rsqr2; %rsquared
rsqr3 = rsqr1/(nobs-tnvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr3/rsqr2); % rbar-squared
results.tnvar=tnvar;

parm = [beta;rhos;sige];
bout= [beta;rhos];
results.lik =f2_sar2_panel(parm,index,ywith,wywith1,wywith2,xwith,W,N,T);

% step 5) Determine asymptotic t-stats based on information matrix
xpx = zeros(nvar+3,nvar+3);
ysum1=zeros(nvar,1);
ysum2=zeros(nvar,1);
ysom1=0;
ysom2=0;
ysom12=0;
ytr1=0;
ytr2=0;
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    W1=W;
    W2=W;
    for i=1:N
        if (index((t-1)*N+i,1)==0) W2(i,1:N)=zeros(1,N);
        else W1(i,1:N)=zeros(1,N);
        end
    end
    B = eye(N) - rho1*W1-rho2*W2; 
    BI = inv(B); WB1 = W1*BI; WB2 = W2*BI;
    ysum1=ysum1+(1/sige)*xwith([t1:t2],:)'*WB1*xwith([t1:t2],:)*beta;
    ysum2=ysum2+(1/sige)*xwith([t1:t2],:)'*WB2*xwith([t1:t2],:)*beta;
    ysom1=ysom1+(1/sige)*beta'*xwith([t1:t2],:)'*WB1'*WB1*xwith([t1:t2],:)*beta +trace(WB1*WB1 + WB1'*WB1);
    ysom2=ysom2+(1/sige)*beta'*xwith([t1:t2],:)'*WB2'*WB2*xwith([t1:t2],:)*beta +trace(WB2*WB2 + WB2'*WB2);
    ysom12=ysom12+(1/sige)*beta'*xwith([t1:t2],:)'*WB1'*WB2*xwith([t1:t2],:)*beta +trace(WB1*WB2 + WB1'*WB2);
    ytr1=ytr1+(1/sige)*trace(WB1);
    ytr2=ytr2+(1/sige)*trace(WB2);
end
% bhat,bhat
xpx(1:nvar,1:nvar) = (1/sige)*(xwith'*xwith);
% bhat,rhos
xpx(1:nvar,nvar+1) = ysum1;
xpx(nvar+1,1:nvar) = xpx(1:nvar,nvar+1)'; 
xpx(1:nvar,nvar+2) = ysum2;
xpx(nvar+2,1:nvar) = xpx(1:nvar,nvar+2)'; 
% rho,rho
xpx(nvar+1,nvar+1) = ysom1;
xpx(nvar+2,nvar+2) = ysom2;
xpx(nvar+1,nvar+2) = ysom12;
xpx(nvar+2,nvar+1) = ysom12;
%sige,sige
xpx(nvar+3,nvar+3) = nobs/(2*sige*sige);
% rhos,sige
xpx(nvar+1,nvar+3) = ytr1;  
xpx(nvar+3,nvar+1) = ytr1;
xpx(nvar+2,nvar+3) = ytr2;
xpx(nvar+3,nvar+2) = ytr2;
xpxi = xpx\eye(size(xpx));
tmp = diag(xpxi(1:nvar+2,1:nvar+2));
tmp = bout./(sqrt(tmp));
results.tstat = tmp;

% Regime differences
results.dif=rho1-rho2;
covar=xpxi(nvar+1,nvar+1)+xpxi(nvar+2,nvar+2)-xpxi(nvar+1,nvar+2)-xpxi(nvar+2,nvar+1);
results.tdif=results.dif/sqrt(covar);

% return stuff
results.y = y;
results.nobs = nobs; 
results.nvar = nvar;
results.rmax = rmax;      
results.rmin = rmin;
results.time = etime(clock,timet);