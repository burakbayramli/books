function serialspatial(y,x,W,N,T,vnames)
% PURPOSE: computes model for spatial panel (N regions*T time periods) with
% serial and spatial error autocorrelation y = XB + u,  u(t) = lambda*W*u(t)+rho*u(t-1)+e, 
% Supply data sorted first by time and then by spatial units, so first region 1,
% region 2, et cetera, in the first year, then region 1, region 2, et
% cetera in the second year, and so on
% -------------------------------------------------------------------------
% written by: J.Paul Elhorst 5/2008
% University of Groningen
% Faculty of Economics and Business
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCES: 
% Elhorst J.P. (2008) Serial and spatial autocorrelation. Economics Letters
% http://dx.doi.org/10.1016/j.econlet.2008.03.009
% -------------------------------------------------------------------------
%  where: y = dependent variable vector
%         x = independent variables matrix 
%         W = spatial weights matrix (standardized)
%         N = number of spatial units
%         T = number of points in time
%         vnames  = an optional vector of variable names of y and x
% -------------------------------------------------------------------------
fid=1;
nflag = 0;
if nargin == 6
    [vsize junk] = size(vnames); % user may supply a blank argument
    if vsize > 0
        nflag = 1;          
    end
end

[s1 s2]=size(x);
K=s2;

% handling of vnames
Vname = 'Variable';
 for i=1:K
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;

 if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= K+1
 fprintf(fid,'Wrong # of variable names in prt_sp -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = 'Variable';
 for i=1:K
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
 end; % end of if-else
end; % end of nflag issue

% add spatial rho and lambda parameter names
    Vname = strvcat(Vname,'ser.aut','spat.aut.');

options = optimset('fmincon');
options.MaxFunEvals=1000;
options.LargeScale='off';
options.TolFun=0.0001;
options.MaxIter=1000;
options.TolX=0.005;

[V,D]=eig(W);
w=diag(D);
wmin=min(w);
rmin=wmin;
lb=[-0.995;rmin+0.005];
ub=[0.995;0.995];
CON=[1 rmin;-1 rmin;1 1;-1 1];
CONB=[1;1;1;1];
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    wy(t1:t2,1)=W*y(t1:t2,1);
    wx(t1:t2,:)=W*x(t1:t2,:);
end
parm=[0.1;0.1];
para=fmincon('f_serialspat',parm,CON,CONB,[],[],lb,ub,[],options,y,wy,x,wx,N,T,w,V);
rho=para(1);
lambda=para(2);
for i=1:N
    eigw(i,1)=(1-(rho/(1-lambda*w(i)))^2)^(0.5);
end
P=V*diag(eigw);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    if (t==1)
        ytr(t1:t2,1)=P*(y(t1:t2)-lambda*wy(t1:t2));
        xtr(t1:t2,:)=P*(x(t1:t2,:)-lambda*wx(t1:t2,:));
    else
        ytr(t1:t2,1)=y(t1:t2)-lambda*wy(t1:t2)-rho*y(t1-N:t2-N);
        xtr(t1:t2,:)=x(t1:t2,:)-lambda*wx(t1:t2,:)-rho*x(t1-N:t2-N,:);
    end
end
beta=inv(xtr'*xtr)*xtr'*ytr;
res=ytr-xtr*beta;
epe=res'*res;
si2=epe/(N*T);
parmout=[beta; rho ; lambda];
parmerror=[rho ; lambda ; si2];
logl=f2_serialspat(parmerror,y,wy,x,wx,N,T,w,V);
hessn=zeros(K+3,K+3);
hessn(1:K,1:K)=(1/si2)*(xtr'*xtr);
hessn(K+1:K+3,K+1:K+3)=hessian('f2_serialspat',parmerror,y,wy,x,wx,N,T,w,V);

if hessn(K+3,K+3) == 0
 hessn(K+3,K+3) = 1/si2;  % this is a hack for very large models that 
end;                             % should not affect inference in these cases

npar=K+2;
xpxi = inv(hessn); 
xpxi = diag(xpxi(1:npar,1:npar));
zip = find(xpxi <= 0);

if length(zip) > 0
xpxi(zip,1) = 1;
fprintf(1,'panel: negative or zero variance from numerical hessian \n');
fprintf(1,'panel: replacing t-stat with 0 \n');
end;

tstat = parmout./sqrt(xpxi);

if length(zip) ~= 0
tstat(zip,1) = 0;
end;

yme=ytr-mean(ytr);
rsqr1=yme'*yme;
results.rsqr=1.0-epe/rsqr1; %rsquared
rsqr3 = epe/(N*T-npar);
rsqr2 = rsqr1/(N*T-1.0);
results.rbar = 1 - (rsqr3/rsqr2); % rbar-squared

%printen
fprintf(fid,'\n');
fprintf(fid,'model with both serial and spatial autocorrelation\n');
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
fprintf(fid,'R-squared          = %9.4f   \n',results.rsqr);
fprintf(fid,'Rbar-squared       = %9.4f   \n',results.rbar);
fprintf(fid,'sigma^2            = %9.4f   \n',si2);
fprintf(fid,'log-likelihood     = %16.8g  \n',logl);
fprintf(fid,'***************************************************************\n');
% now print coefficient estimates, t-statistics and probabilities
tout = norm_prb(tstat); % find asymptotic z (normal) probabilities
tmp = [parmout tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 'Asymptot t-stat'; pstring = 'z-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);