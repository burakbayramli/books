clear all;

A=wk1read('cigardemo.wk1',1,0);
W1=wk1read('spat-Sym-US.wk1');
% Matlab cannot read excel files, but Excel can save data als lotus files with the extension wk1
% 1,0 means skip the first row (e.g. if the file contains variable names
% but no columns (if the file contains region names in the first column, this should be 1)
%
% program to estimate panel data model (N regions*T time periods) with
% serial lagged dependent variable, spatial error autocorrelation and
% set of regressors.
% data sorted first by time and then by spatial units, so first region 1,
% region 2, et cetera, in the first year, then region 1, region 2, et
% cetera in the second year, and so on
% It is assumed that variables are given in levels; this routine transforms
% the data into first-differences
%
% written by: J.Paul Elhorst 9/2004
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@eco.rug.nl
%
% This demo produces the estimation results in table 2 of my paper
% "Unconditional Maximum Likelihood Estimation of 
% Linear and Log-linear Dynamic Models for Spatial Panels"
% Geographical Analysis (2005), No. 1, pp. 
%
% The small deviations in the paramater estimates compared to those in table 2
% of my paper are the result of improved programming procedures
%
% Note: some routines or functions should be downloaded from
% James. P LeSage's website www.spatial-econometrics.com
%
% dimensions of the problem
N=46; % number of spatial units
T=6; % number of time periods
K=3; % number of explanatory variables, constant should not be included
m=63; % Value that indicates the period of time between the first observation in the sample and the moment the process being modeled has started in the past
M=2*m-1;
nobs=N*T;
% normalization of W, but W is kept symmetric
lambda=eig(W1);
W=W1/lambda(N);
lambda=eig(W);
y1=A(:,[3]); % column number in the data matrix that corresponds to the dependent variable
x1=A(:,[4,5,6]); % column numbers in the data matrix that correspond to the independent variables, no constant because it will be eliminated
% Transformation of data into first-differences
dy=y1(N+1:N*T,1)-y1(1:(N*T-N),1); 
nobs=N*T-N; % length of dy becomes N*T-N, number of observations decreases
dy1=zeros(nobs,1);
dy1(N+1:nobs,1)=y1(N+1:nobs,1)-y1(1:(N*T-2*N),1); % dy1(1:N,1) remains zero, because serial lagged depedent variable is not observed at t-2
dx=x1(N+1:N*T,:)-x1(1:(N*T-N),:);
T=T-1; % number of time periods has decreased by 1 due to transformation into first-differences
%
% To include time period fixed effects, the variables in the
% first-differenced regression equation should be taken in deviation from
% their first-differenced averages over all cross-sectional units within
% each time period.
%
meany=zeros(T,1);
meany1=zeros(T,1);
meanx=zeros(T,K);
for i=1:T
   t1=1+(i-1)*N;t2=i*N;
   ym=dy([t1:t2],1);
   y1m=dy1([t1:t2],1);
   xm=dx([t1:t2],:);
   meany(i)=mean(ym);
   meany1(i)=mean(y1m);
   temp1=mean(xm);
   for j=1:K
      meanx(i,j)=temp1(j);
   end;
end;
ee=ones(N,1);
y=dy-kron(meany,ee);
y1=dy1-kron(meany1,ee);
x=dx-kron(meanx,ee);
wy=y;
wy1=y1;
for t=1:T
   ti=1+(t-1)*N;tj=t*N;
   wy(ti:tj)=W*y(ti:tj);
   wy1(ti:tj)=W*y1(ti:tj);
end;
%
% Approximation according to Bhargava and Sargan
% Due to time period fixed effects, no pie_null
%
warning off MATLAB:divideByZero
warning off MATLAB:singularMatrix
nvar=2*K+3;
xpie=zeros(nobs,K);
for t=1:T
   for i=1:N
   ti=i+(t-1)*N;
   xpie(i,:)=xpie(i,:)+x(ti,:);
end;end;
x(1:N,1:K)=zeros(N,K);
X=[xpie x];
wx=x;
wxpie=xpie;
for t=1:T
   ti=1+(t-1)*N;tj=t*N;
   wx(ti:tj,:)=W*x(ti:tj,:);
   wxpie(ti:tj,:)=W*xpie(ti:tj,:);
end;
WX=[wxpie wx];
% 
G=zeros(T,T);
for t=2:T
   G(t-1,t)=-1;
   G(t,t-1)=-1;
   G(t,t)=2;
end;
G(1,1)=0;
GC=(1-T)*inv(G);
G(1,1)=1;
GV=inv(G)-GC;
%
% Starting values to be specified by the researcher
%
gam=0.60;
del=0.1;
teta=0.0;
pie=zeros(K,1);
beta=[-0.27;0.28;0.24]; % This number of initial values should be equal to K
options.Display='off';
options.TolFun=0.0001;
options.MaxFunEvals=250;
options.MaxIter=1000;
options.TolX=0.005;
tetaoud=0;deloud=0;gamoud=gam;iter=0;converge=1.0;criteria=0.001;itermax=25;
t1=clock;
while (converge>criteria & iter < itermax)
   B=eye(N)-del*W;
   V=teta^2*B^2+2*(1+gam^M)/(1+gam)*eye(N);
   detinv=inv(eye(N)+T*(V-eye(N)));
   ee=y-xpie*pie-gam*y1-x*beta;
   ew=wy-wxpie*pie-gam*wy1-wx*beta;
   ed=ee-del*ew;
   edt2=0;
   xhx=zeros(2*K,2*K);
   xhy=zeros(2*K,1);
   % determination s2, pie and beta
   for ti=1:T
      ii=(ti-1)*N+1;iend=ti*N;
      ei=ed(ii:iend);
      xi=X(ii:iend,:)-del*WX(ii:iend,:);
      for tj=1:T
         ij=(tj-1)*N+1;jend=tj*N;
         ej=ed(ij:jend);        
         xj=X(ij:jend,:)-del*WX(ij:jend,:);
         yj=y(ij:jend,1)-del*wy(ij:jend,1)-gam*y1(ij:jend,1)+del*gam*wy1(ij:jend,1);
         matv=GC(ti,tj)*detinv+GV(ti,tj)*detinv*V;
         et=ei'*matv*ej;
         edt2=edt2+et;
         etx=xi'*matv*xj;
         xhx=xhx+etx;
         ety=xi'*matv*yj;
         xhy=xhy+ety;
      end;
   end;
   si2=edt2/(N*T);
   bpb=inv(xhx)*xhy;
   del=fminbnd('f_demodynerrorBS_del',-1,1,options,y,y1,wy,wy1,x,wx,xpie,wxpie,W,lambda,N,T,m,K,gam,teta,bpb,si2);
   gam=fminbnd('f_demodynerrorBS_gam',-1,1,options,y,y1,wy,wy1,x,wx,xpie,wxpie,W,lambda,N,T,m,K,del,teta,bpb,si2);
   teta=fminbnd('f_demodynerrorBS_teta',0,3,options,y,y1,wy,wy1,x,wx,xpie,wxpie,W,lambda,N,T,m,K,gam,del,bpb,si2);
   ball=[del;gam;teta]
   iter=iter+1;
   converge=abs(del-deloud)+abs(gam-gamoud)+abs(teta-tetaoud);
   deloud=del;
   gamoud=gam;
   tetaoud=teta;
end;
iter
bpb
btemp=[bpb;gam;del;teta];
parm=[btemp;si2];
functie=f2_demodynerrorBS(parm,y,y1,wy,wy1,x,wx,xpie,wxpie,W,lambda,N,T,m,K)
hessn = hessian('f2_demodynerrorBS',parm,y,y1,wy,wy1,x,wx,xpie,wxpie,W,lambda,N,T,m,K);
t2=clock;
time_used=t2-t1

if hessn(nvar+1,nvar+1) == 0
 hessn(nvar+1,nvar+1) = 1/si2;  % this is a hack for very large models that 
end;                             % should not affect inference in these cases

xpxi = inv(hessn); 
xpxi = diag(xpxi(1:nvar,1:nvar));
zip = find(xpxi <= 0);

if length(zip) > 0
xpxi(zip,1) = 1;
fprintf(1,'panel: negative or zero variance from numerical hessian \n');
fprintf(1,'panel: replacing t-stat with 0 \n');
end;

btemp=parm(1:nvar);
tstat = btemp./sqrt(xpxi);

if length(zip) ~= 0
tstat(zip,1) = 0;
end;

fid=1; 
vnames=strvcat('logcit','pie1','pie2','pie3','logp','logpn','logy','logcit[-1]','spataut','del'); % should be changed if x is changed
Vname = 'Variable';
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 else
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;end;
 
fprintf(fid,'\n');
fprintf(fid,'Model Estimates \n');
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
fprintf(fid,'sigma^2         = %16.8f   \n',si2);
fprintf(fid,'log-likelihood  = %16.8g  \n',functie);
fprintf(fid,'Nobs, Nvars     = %6d,%6d \n',nobs,nvar);
fprintf(fid,'# iterations    = %6d     \n',iter);

% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(tstat); % find asymptotic z (normal) probabilities
tmp = [btemp tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Asymptot t-stat'; pstring = 'z-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
%__________________________________________________________________________
%
% Approximation according to Nerlove and Balestra
%
nvar=K+2;
varx=cov(x(N+1:nobs,:))
t1=clock;
del=0.1;
gam=0.60;
beta=[-0.27;0.28;0.24]; % This number of initial values should be equal to K
si2=0.0010;
yx=[y y1 x];
wyx=[wy wy1 wx];
gamoud=gam;deloud=del;iter=0;converge=1.0;criteria=0.001;itermax=25;
while ( converge>criteria & iter < itermax)
   del=fminbnd('f_demodynerrorNB_del',-1,+1,options,yx,wyx,W,lambda,N,T,varx,gam,si2,beta,m,K);
   gam=fminbnd('f_demodynerrorNB_gam',0,1,options,yx,wyx,W,lambda,N,T,varx,del,si2,beta,m,K);
   si2=fminbnd('f_demodynerrorNB_si2',0.0001,0.5,options,yx,wyx,W,lambda,N,T,varx,del,gam,beta,m,K);
   bnuis=[del;gam;si2]
   btemp=fminsearch('f_demodynerrorNB',beta,options,yx,wyx,W,lambda,N,T,varx,del,gam,si2,m,K);
   beta=btemp;
   iter=iter+1
   converge=abs(gam-gamoud)+abs(del-deloud);
   gamoud=gam;
   deloud=del;
end;
functie=f_demodynerrorNB(btemp,yx,wyx,W,lambda,N,T,varx,del,gam,si2,m,K)
btemp=[gam;btemp;del];
parm=[btemp;si2];
hessn = hessian('f2_demodynerrorNB',parm,yx,wyx,W,lambda,N,T,varx,m,K);
hessn
t2=clock;
time_used=t2-t1

if hessn(nvar+1,nvar+1) == 0
 hessn(nvar+1,nvar+1) = 1/si2;  % this is a hack for very large models that 
end;                             % should not affect inference in these cases

xpxi = inv(hessn); 
xpxi = diag(xpxi(1:nvar,1:nvar));
zip = find(xpxi <= 0);

if length(zip) > 0
xpxi(zip,1) = 1;
fprintf(1,'panel: negative or zero variance from numerical hessian \n');
fprintf(1,'panel: replacing t-stat with 0 \n');
end;

btemp=btemp(1:nvar);
tstat = btemp./sqrt(xpxi);

if length(zip) ~= 0
tstat(zip,1) = 0;
end;

fid=1; 
vnames=strvcat('logcit','logcit[-1]','logp','logpn','logy','spataut'); % should be changed if x is changed
Vname = 'Variable';
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_spat -- check vnames argument \n');
 else
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;end;
 
fprintf(fid,'\n');
fprintf(fid,'Model Estimates \n');
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
fprintf(fid,'sigma^2         = %16.8f   \n',si2);
fprintf(fid,'log-likelihood  = %16.8g  \n',functie);
fprintf(fid,'Nobs, Nvars     = %6d,%6d \n',nobs,nvar);
fprintf(fid,'# iterations    = %6d     \n',iter);

% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(tstat); % find asymptotic z (normal) probabilities
tmp = [btemp tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Asymptot t-stat'; pstring = 'z-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);
%--------------------------------------------------------------------------
function llike=f_demodynerrorBS_del(del,y,y1,wy,wy1,x,wx,xpie,wxpie,W,lambda,N,T,m,K,gam,teta,bpb,si2)
% computes log-likelihood function
% approximation according to Bhargava and Sargan
pie=bpb(1:K);
beta=bpb(K+1:2*K);
M=2*m-1;
G=zeros(T,T);
for t=2:T
   G(t-1,t)=-1;
   G(t,t-1)=-1;
   G(t,t)=2;
end;
G(1,1)=0;
GC=(1-T)*inv(G);
G(1,1)=1;
GV=inv(G)-GC;
B=eye(N)-del*W;
V=teta^2*B^2+2*(1+gam^M)/(1+gam)*eye(N);
detinv=inv(eye(N)+T*(V-eye(N)));
ee=y-xpie*pie-gam*y1-x*beta;
ew=wy-wxpie*pie-gam*wy1-wx*beta;
ed=ee-del*ew;
edt2=0;
for ti=1:T
   ii=(ti-1)*N+1;iend=ti*N;
   ei=ed(ii:iend);
   for tj=1:T
      ij=(tj-1)*N+1;jend=tj*N;
      ej=ed(ij:jend);        
      matv=GC(ti,tj)*detinv+GV(ti,tj)*detinv*V;
      et=ei'*matv*ej;
      edt2=edt2+et;
   end;
end;
som1=zeros(N,1);
som2=zeros(N,1);
for i=1:N
   t1=1-del*lambda(i);
   som1(i)=log(t1);
   som2(i)=log(1-T+2*T*(1+gam^m)/(1+gam)+T*(teta*t1)^2);
end;
tmp2=1/(2*si2);
llike=(N*T/2)*log(2*pi*si2)-T*sum(som1)+0.5*sum(som2)+tmp2*edt2;
%--------------------------------------------------------------------------
function llike=f_demodynerrorBS_gam(gam,y,y1,wy,wy1,x,wx,xpie,wxpie,W,lambda,N,T,m,K,del,teta,bpb,si2)
% computes log-likelihood function
% approximation according to Bhargava and Sargan
pie=bpb(1:K);
beta=bpb(K+1:2*K);
M=2*m-1;
G=zeros(T,T);
for t=2:T
   G(t-1,t)=-1;
   G(t,t-1)=-1;
   G(t,t)=2;
end;
G(1,1)=0;
GC=(1-T)*inv(G);
G(1,1)=1;
GV=inv(G)-GC;
B=eye(N)-del*W;
V=teta^2*B^2+2*(1+gam^M)/(1+gam)*eye(N);
detinv=inv(eye(N)+T*(V-eye(N)));
ee=y-xpie*pie-gam*y1-x*beta;
ew=wy-wxpie*pie-gam*wy1-wx*beta;
ed=ee-del*ew;
edt2=0;
for ti=1:T
   ii=(ti-1)*N+1;iend=ti*N;
   ei=ed(ii:iend);
   for tj=1:T
      ij=(tj-1)*N+1;jend=tj*N;
      ej=ed(ij:jend);        
      matv=GC(ti,tj)*detinv+GV(ti,tj)*detinv*V;
      et=ei'*matv*ej;
      edt2=edt2+et;
   end;
end;
som1=zeros(N,1);
som2=zeros(N,1);
for i=1:N
   t1=1-del*lambda(i);
   som1(i)=log(t1);
   som2(i)=log(1-T+2*T*(1+gam^m)/(1+gam)+T*(teta*t1)^2);
end;
tmp2=1/(2*si2);
llike=(N*T/2)*log(2*pi*si2)-T*sum(som1)+0.5*sum(som2)+tmp2*edt2;
%--------------------------------------------------------------------------
function llike=f_demodynerrorBS_teta(teta,y,y1,wy,wy1,x,wx,xpie,wxpie,W,lambda,N,T,m,K,gam,del,bpb,si2)
% computes log-likelihood function
% approximation according to Bhargava and Sargan
pie=bpb(1:K);
beta=bpb(K+1:2*K);
M=2*m-1;
G=zeros(T,T);
for t=2:T
   G(t-1,t)=-1;
   G(t,t-1)=-1;
   G(t,t)=2;
end;
G(1,1)=0;
GC=(1-T)*inv(G);
G(1,1)=1;
GV=inv(G)-GC;
B=eye(N)-del*W;
V=teta^2*B^2+2*(1+gam^M)/(1+gam)*eye(N);
detinv=inv(eye(N)+T*(V-eye(N)));
ee=y-xpie*pie-gam*y1-x*beta;
ew=wy-wxpie*pie-gam*wy1-wx*beta;
ed=ee-del*ew;
edt2=0;
for ti=1:T
   ii=(ti-1)*N+1;iend=ti*N;
   ei=ed(ii:iend);
   for tj=1:T
      ij=(tj-1)*N+1;jend=tj*N;
      ej=ed(ij:jend);        
      matv=GC(ti,tj)*detinv+GV(ti,tj)*detinv*V;
      et=ei'*matv*ej;
      edt2=edt2+et;
   end;
end;
som1=zeros(N,1);
som2=zeros(N,1);
for i=1:N
   t1=1-del*lambda(i);
   som1(i)=log(t1);
   som2(i)=log(1-T+2*T*(1+gam^m)/(1+gam)+T*(teta*t1)^2);
end;
tmp2=1/(2*si2);
llike=(N*T/2)*log(2*pi*si2)-T*sum(som1)+0.5*sum(som2)+tmp2*edt2;
%--------------------------------------------------------------------------
function llike=f2_demodynerrorBS(parm,y,y1,wy,wy1,x,wx,xpie,wxpie,W,lambda,N,T,m,K)
% computes log-likelihood function
% approximation according to Bhargava and Sargan
pie=parm(1:K);
beta=parm(K+1:2*K);
gam=parm(2*K+1);
del=parm(2*K+2);
teta=parm(2*K+3);
si2=parm(2*K+4);
M=2*m-1;
G=zeros(T,T);
for t=2:T
   G(t-1,t)=-1;
   G(t,t-1)=-1;
   G(t,t)=2;
end;
G(1,1)=0;
GC=(1-T)*inv(G);
G(1,1)=1;
GV=inv(G)-GC;
B=eye(N)-del*W;
V=teta^2*B^2+2*(1+gam^M)/(1+gam)*eye(N);
detinv=inv(eye(N)+T*(V-eye(N)));
ee=y-xpie*pie-gam*y1-x*beta;
ew=wy-wxpie*pie-gam*wy1-wx*beta;
ed=ee-del*ew;
edt2=0;
for ti=1:T
   ii=(ti-1)*N+1;iend=ti*N;
   ei=ed(ii:iend);
   for tj=1:T
      ij=(tj-1)*N+1;jend=tj*N;
      ej=ed(ij:jend);        
      matv=GC(ti,tj)*detinv+GV(ti,tj)*detinv*V;
      et=ei'*matv*ej;
      edt2=edt2+et;
   end;
end;
som1=zeros(N,1);
som2=zeros(N,1);
for i=1:N
   t1=1-del*lambda(i);
   som1(i)=log(t1);
   som2(i)=log(1-T+2*T*(1+gam^m)/(1+gam)+T*(teta*t1)^2);
end;
tmp2=1/(2*si2);
llike=(N*T/2)*log(2*pi*si2)-T*sum(som1)+0.5*sum(som2)+tmp2*edt2;
%--------------------------------------------------------------------------
function llike=f_demodynerrorNB(b,yx,wyx,W,lambda,N,T,varx,del,gam,si2,m,K)
% computes log-likelihood function
% Approximation according to Nerlove and Balestra
beta=b(1:K);
M=2*m-1;
G=zeros(T,T);
for t=2:T
   G(t-1,t)=-1;
   G(t,t-1)=-1;
   G(t,t)=2;
end;
G(1,1)=0;
GC=(1-T)*inv(G);
G(1,1)=1;
GV=inv(G)-GC;
B=eye(N)-del*W;
BVB=beta'*varx*beta/si2;
V=2*(1+gam^M)/(1+gam)*eye(N)+((1-gam^m)/(1-gam))^2*BVB*B*B';
detinv=inv(eye(N)+T*(V-eye(N)));
som1=zeros(N,1);
som2=zeros(N,1);
for i=1:N
   t1=1-del*lambda(i);
   som1(i)=log(t1);
   som2(i)=log(1-T+2*T*(1+gam^M)/(1+gam)+T*BVB*((1-gam^m)/(1-gam))^2*t1^2);
end;
ee=yx(:,1)-gam*yx(:,2)-yx(:,3:2+K)*beta;
ew=wyx(:,1)-gam*wyx(:,2)-wyx(:,3:2+K)*beta;
ed=ee-del*ew;
edt2=0;
% transformation residuals
for ti=1:T
   ii=(ti-1)*N+1;iend=ti*N;
   ei=ed(ii:iend);
   for tj=ti:T
      ij=(tj-1)*N+1;jend=tj*N;
      ej=ed(ij:jend);
      matv=GC(ti,tj)*detinv+GV(ti,tj)*detinv*V;
      if (ti==tj) et=ei'*matv*ej;else et=2*ei'*matv*ej;end;
      edt2=edt2+et;
   end;
end;
tmp2=1/(2*si2);
llike=(N*T/2)*log(2*pi*si2)-T*sum(som1)+0.5*sum(som2)+tmp2*edt2;
%--------------------------------------------------------------------------
function llike=f_demodynerrorNB_del(del,yx,wyx,W,lambda,N,T,varx,gam,si2,beta,m,K)
% computes log-likelihood function
% Approximation according to Nerlove and Balestra
M=2*m-1;
G=zeros(T,T);
for t=2:T
   G(t-1,t)=-1;
   G(t,t-1)=-1;
   G(t,t)=2;
end;
G(1,1)=0;
GC=(1-T)*inv(G);
G(1,1)=1;
GV=inv(G)-GC;
B=eye(N)-del*W;
BVB=beta'*varx*beta/si2;
V=2*(1+gam^M)/(1+gam)*eye(N)+((1-gam^m)/(1-gam))^2*BVB*B*B';
detinv=inv(eye(N)+T*(V-eye(N)));
som1=zeros(N,1);
som2=zeros(N,1);
for i=1:N
   t1=1-del*lambda(i);
   som1(i)=log(t1);
   som2(i)=log(1-T+2*T*(1+gam^M)/(1+gam)+T*BVB*((1-gam^m)/(1-gam))^2*t1^2);
end;
ee=yx(:,1)-gam*yx(:,2)-yx(:,3:2+K)*beta;
ew=wyx(:,1)-gam*wyx(:,2)-wyx(:,3:2+K)*beta;
ed=ee-del*ew;
edt2=0;
% transformation residuals
for ti=1:T
   ii=(ti-1)*N+1;iend=ti*N;
   ei=ed(ii:iend);
   for tj=ti:T
      ij=(tj-1)*N+1;jend=tj*N;
      ej=ed(ij:jend);
      matv=GC(ti,tj)*detinv+GV(ti,tj)*detinv*V;
      if (ti==tj) et=ei'*matv*ej;else et=2*ei'*matv*ej;end;
      edt2=edt2+et;
   end;
end;
tmp2=1/(2*si2);
llike=(N*T/2)*log(2*pi*si2)-T*sum(som1)+0.5*sum(som2)+tmp2*edt2;
%--------------------------------------------------------------------------
function llike=f_demodynerrorNB_gam(gam,yx,wyx,W,lambda,N,T,varx,del,si2,beta,m,K)
% computes log-likelihood function
% Approximation according to Nerlove and Balestra
M=2*m-1;
G=zeros(T,T);
for t=2:T
   G(t-1,t)=-1;
   G(t,t-1)=-1;
   G(t,t)=2;
end;
G(1,1)=0;
GC=(1-T)*inv(G);
G(1,1)=1;
GV=inv(G)-GC;
B=eye(N)-del*W;
BVB=beta'*varx*beta/si2;
V=2*(1+gam^M)/(1+gam)*eye(N)+((1-gam^m)/(1-gam))^2*BVB*B*B';
detinv=inv(eye(N)+T*(V-eye(N)));
som1=zeros(N,1);
som2=zeros(N,1);
for i=1:N
   t1=1-del*lambda(i);
   som1(i)=log(t1);
   som2(i)=log(1-T+2*T*(1+gam^M)/(1+gam)+T*BVB*((1-gam^m)/(1-gam))^2*t1^2);
end;
ee=yx(:,1)-gam*yx(:,2)-yx(:,3:2+K)*beta;
ew=wyx(:,1)-gam*wyx(:,2)-wyx(:,3:2+K)*beta;
ed=ee-del*ew;
edt2=0;
% transformation residuals
for ti=1:T
   ii=(ti-1)*N+1;iend=ti*N;
   ei=ed(ii:iend);
   for tj=ti:T
      ij=(tj-1)*N+1;jend=tj*N;
      ej=ed(ij:jend);
      matv=GC(ti,tj)*detinv+GV(ti,tj)*detinv*V;
      if (ti==tj) et=ei'*matv*ej;else et=2*ei'*matv*ej;end;
      edt2=edt2+et;
   end;
end;
tmp2=1/(2*si2);
llike=(N*T/2)*log(2*pi*si2)-T*sum(som1)+0.5*sum(som2)+tmp2*edt2;
%--------------------------------------------------------------------------
function llike=f_demodynerrorNB_si2(si2,yx,wyx,W,lambda,N,T,varx,del,gam,beta,m,K)
% computes log-likelihood function
% Approximation according to Nerlove and Balestra
M=2*m-1;
G=zeros(T,T);
for t=2:T
   G(t-1,t)=-1;
   G(t,t-1)=-1;
   G(t,t)=2;
end;
G(1,1)=0;
GC=(1-T)*inv(G);
G(1,1)=1;
GV=inv(G)-GC;
B=eye(N)-del*W;
BVB=beta'*varx*beta/si2;
V=2*(1+gam^M)/(1+gam)*eye(N)+((1-gam^m)/(1-gam))^2*BVB*B*B';
detinv=inv(eye(N)+T*(V-eye(N)));
som1=zeros(N,1);
som2=zeros(N,1);
for i=1:N
   t1=1-del*lambda(i);
   som1(i)=log(t1);
   som2(i)=log(1-T+2*T*(1+gam^M)/(1+gam)+T*BVB*((1-gam^m)/(1-gam))^2*t1^2);
end;
ee=yx(:,1)-gam*yx(:,2)-yx(:,3:2+K)*beta;
ew=wyx(:,1)-gam*wyx(:,2)-wyx(:,3:2+K)*beta;
ed=ee-del*ew;
edt2=0;
% transformation residuals
for ti=1:T
   ii=(ti-1)*N+1;iend=ti*N;
   ei=ed(ii:iend);
   for tj=ti:T
      ij=(tj-1)*N+1;jend=tj*N;
      ej=ed(ij:jend);
      matv=GC(ti,tj)*detinv+GV(ti,tj)*detinv*V;
      if (ti==tj) et=ei'*matv*ej;else et=2*ei'*matv*ej;end;
      edt2=edt2+et;
   end;
end;
tmp2=1/(2*si2);
llike=(N*T/2)*log(2*pi*si2)-T*sum(som1)+0.5*sum(som2)+tmp2*edt2;
%--------------------------------------------------------------------------
function llike=f2_demodynerrorNB(b,yx,wyx,W,lambda,N,T,varx,m,K)
% computes log-likelihood function
% Approximation according to Nerlove and Balestra
gam=b(1);
beta=b(2:K+1);
del=b(K+2);
si2=b(K+3);
M=2*m-1;
G=zeros(T,T);
for t=2:T
   G(t-1,t)=-1;
   G(t,t-1)=-1;
   G(t,t)=2;
end;
G(1,1)=0;
GC=(1-T)*inv(G);
G(1,1)=1;
GV=inv(G)-GC;
B=eye(N)-del*W;
BVB=beta'*varx*beta/si2;
V=2*(1+gam^M)/(1+gam)*eye(N)+((1-gam^m)/(1-gam))^2*BVB*B*B';
detinv=inv(eye(N)+T*(V-eye(N)));
som1=zeros(N,1);
som2=zeros(N,1);
for i=1:N
   t1=1-del*lambda(i);
   som1(i)=log(t1);
   som2(i)=log(1-T+2*T*(1+gam^M)/(1+gam)+T*BVB*((1-gam^m)/(1-gam))^2*t1^2);
end;
ee=yx(:,1)-gam*yx(:,2)-yx(:,3:2+K)*beta;
ew=wyx(:,1)-gam*wyx(:,2)-wyx(:,3:2+K)*beta;
ed=ee-del*ew;
edt2=0;
% transformation residuals
for ti=1:T
   ii=(ti-1)*N+1;iend=ti*N;
   ei=ed(ii:iend);
   for tj=ti:T
      ij=(tj-1)*N+1;jend=tj*N;
      ej=ed(ij:jend);
      matv=GC(ti,tj)*detinv+GV(ti,tj)*detinv*V;
      if (ti==tj) et=ei'*matv*ej;else et=2*ei'*matv*ej;end;
      edt2=edt2+et;
   end;
end;
tmp2=1/(2*si2);
llike=(N*T/2)*log(2*pi*si2)-T*sum(som1)+0.5*sum(som2)+tmp2*edt2;