function results = sarar_panel_FE_LY(y,x,W1,W2,N,info)
% PURPOSE: computes general Spatial Model estimates for fixed effects panel
% data models.

%    model: y = rho*W1*y + X*b + u,  u = lam*W2*u + e

% This program has been developed only for individual effects (no time
% effects)
% This function uses the data transformation proposed by Lung-Fei Lee and
% Jihai Yu (Journal of Econometrics, 2010, 154, 165-185) 

% Input of the function should be original data. The transformation is 
% implemented just below. 

% Supply data sorted first by time and then by spatial units, so first region 1,
% region 2, et cetera, in the first year, then region 1, region 2, et
% cetera in the second year, and so on

% ---------------------------------------------------
%  USAGE: results = sarar_panel_FE_LY(y,x,W1,W2,N,info)
%  where: y  = dependent variable vector
%         x  = independent variables matrix (WITHOUT CONSTANT)
%         W1 = spatial weight matrix (standardized) associated to the
%              endogenous spatial lag
%         W2 = spatial weight matrix (standardized) associated to the
%              spatially autocorrelated errors.
%          N = Number of individuals
%      info        = an (optional) structure variable with input options
%      info.parm   = (optional) 2x1 vector of starting values for rho, lambda
%      info.convg  = (optional) convergence criterion (default = 1e-4)
%      info.maxit  = (optional) maximum # of iterations (default = 500)
%      info.lmin   = (optional) minimum lambda to search (default = -0.99)
%      info.lmax   = (optional) maximum lambda to search (default = 0.99)
%      info.rmin   = (optional) minimum rho to search (default = -0.99)
%      info.rmax   = (optional) maximum rho to search (default = 0.99)
%      info.lflag  = 0 for full computation (default = 1, fastest)
%                  = 1 for Pace and Barry 1999 MC approximation (fast for very large problems)
%                  = 2 for Pace and Barry 1998 Spline approximation (medium speed)
%      info.order  = order to use with info.lflag = 1 option (default = 50)
%      info.iter   = iterations to use with info.lflag = 1 option (default = 30)     
%      info.Nhes   = Threshold value under which asymptotic variance matrix is computed using analytical formulas,
%                    N > Nhes asymptotic variance matrix is computed using numerical formulas
%                    (Default NHes=500)
% ---------------------------------------------------
%  RETURNS: a structure 
%         results.meth  = 'sarar_panel_FE_LY'
%         results.beta  = bhat
%         results.rho   = rho
%         results.lam   = lambda
%         results.tstat = asymptotic t-stats (last 2 are rho,lambda)
%         results.yhat  = yhat
%         results.resid = residuals
%         results.sige  = sige = e'(I-L*W)'*(I-L*W)*e/n
%         results.cov   = Variance covariance Matrix of beta, rho and
%                         lambda
%         results.lik   = likelihood function value
%         results.nobs  = nobs
%         results.nvar  = nvars
%         results.y     = y data vector
%         results.iter  = # of iterations taken
%         results.lflag = lflag from input
%         results.liter = info.iter option from input
%         results.order = info.order option from input
%         results.limit = matrix of [rho lower95,logdet approx, upper95] intervals
%                         for the case of lflag = 1
%         results.time1 = time for log determinant calcluation
%         results.time2 = time for eigenvalue calculation
%         results.time3 = time for hessian or information matrix calculation
%         results.time4 = time for optimization


%   Code has been programmed for balanced panel
%--------------------
% Written by N. Debarsy* and C. Ertur** (fall 2009) 
% * University of Namur
%   Centre de recherches en Economie Régionale et Politique Economique (CERPE)
%   Rempart de la vierge, 8
%   5000 Namur, Belgium
%   nicolas.debarsy@fundp.ac.be

%** Université d'Orléans
%   UFR Droit-Economie-Gestion
%   Laboratoire d'Economie d'Orléans - UMR 6221 CNRS
%   Domaine Universitaire
%   Rue de Blois - BP 6739
%   45067 ORLEANS Cedex 2, France
%   cem.ertur@univ-orleans.fr

% This function is partly based on James. P LeSage's function SAC 

% REFERENCES:
% Lee L.-F.; J. Yu (2010), "Estimation of spatial autoregressive panel data
% models with fixed effects", Journal of Econometrics, 154, 165-185.
%----------------------------------------------
rflag = 0;
ldetflag = 1; % default to the fastest method
rflag = 0;
order = 50; liter = 30; % defaults
time1 = 0; 
time2 = 0;
time3 = 0;
results.order = order;
results.liter = liter;
Nhes = 500;
lmin = -0.99;
lmax = 0.99;
rmin = -0.99;
rmax = 0.99;
    parm = [0.5
         0.5];

timet = clock; % start the clock for overall timing
% default options
options = optimset('fminsearch');

if nargin == 6
 if ~isstruct(info)
 error('sarar: must supply the options as a structure variable');
 end;
options.MaxIter = 500;
 fields = fieldnames(info);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'parm')
       parm = info.parm;
	elseif strcmp(fields{i},'convg')
       options.TolFun = info.convg;
    elseif strcmp(fields{i},'maxit')
        options.MaxIter  = info.maxit;
    elseif strcmp(fields{i},'rmin')
        rmin = info.rmin;
    elseif strcmp(fields{i},'rmax')
        rmax = info.rmax;
    elseif strcmp(fields{i},'lmin')
        lmin = info.lmin;
    elseif strcmp(fields{i},'lmax')
        lmax = info.lmax;
    elseif strcmp(fields{i},'Nhes')
        Nhes  = info.Nhes;
    elseif strcmp(fields{i},'lflag')
        ldetflag = info.lflag;
    elseif strcmp(fields{i},'order')
        order = info.order;  results.order = order;
    elseif strcmp(fields{i},'iter')
    liter = info.iter; results.liter = liter;
    end;
 end;
elseif nargin == 5 % use default options
options = optimset('fminsearch');
else
 error('Wrong # of arguments to sarar'); 
end; 


[NT nvar] = size(x); 


% Definition of the number of periods considered
T=NT/N;


[n1 n2] = size(W1);
if n1 ~= n2
error('sac: wrong size weight matrix W1');
elseif n1 ~= N
error('sac: wrong size weight matrix W1');
end;

[n1 n2] = size(W2);
if n1 ~= n2
error('sac: wrong size weight matrix W2');
elseif n1 ~= N
error('sac: wrong size weight matrix W2');
end;

% Transformation of the data according to the Lee and Yu methodology

IT=eye(T);
IN=eye(N);
Jt=ones(T);
Jbar=1/T*Jt;
Q=IT-Jbar;
[V,D]=eig(Q);
D=diag(D);
j=find(D<=0.0001&D>=-0.0001);
V(:,j)=[];
F=V;
TR=kron(F',IN);
% Transformed data
y=TR*y;
x=TR*x;
T=size(y,1)/N; % Effectively T=T-1 since the Lee transformation shrinks the sample. 
results.y = y;      
NT1=length(y);
results.nobs = NT1; results.nvar = nvar;
results.meth = 'sarar_panel_FE_LY';

% do lndet approximation calculations if needed
if ldetflag == 0 % no approximation
t0 = clock;    
out = lndetfull(W1,rmin,rmax);
time1 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det1 = [tt' outi];

t0 = clock;    
out = lndetfull(W2,lmin,lmax);
time1 = time1 + etime(clock,t0);
tt=lmin:.001:lmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det2 = [tt' outi];

elseif ldetflag == 1 % use Pace and Barry, 1999 MC approximation

t0 = clock;    
out = lndetmc(order,liter,W1,rmin,rmax);
time1 = etime(clock,t0);
results.limit = [out.rho out.lo95 out.lndet out.up95];
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det1 = [tt' outi];

t0 = clock;    
out = lndetmc(order,liter,W2,lmin,lmax);
time1 = time1 + etime(clock,t0);
results.limit = [out.rho out.lo95 out.lndet out.up95];
tt=lmin:.001:lmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det2 = [tt' outi];

elseif ldetflag == 2 % use Pace and Barry, 1998 spline interpolation

t0 = clock;
out = lndetint(W1);
time1 = etime(clock,t0);
tt=.001:.001:1; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det1 = [tt' outi];

t0 = clock;
out = lndetint(W2);
time1 = time1 + etime(clock,t0);
tt=.001:.001:1; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
det2 = [tt' outi];

end;

timeo = clock;
[pout,like,exitflag,output]=fminsearch('f_sarar_panel',parm,options,y,x,W1,W2,det1,det2,T);
time4 = etime(clock,timeo);

if exitflag == 0 
fprintf(1,'\n sac: convergence not obtained in %4d iterations \n',output.iterations);
end;
results.iter = output.iterations;

rho = pout(1,1);
lam = pout(2,1);

% fill-in results
It=speye(T);
A=speye(N) - rho*sparse(W1);
B=speye(N) - lam*sparse(W2);
At = kron(It,A);
Bt = kron(It,B);
bx=Bt*x;
b0 = inv(bx'*bx)*(bx'*Bt*At*y);
e = Bt*(At*y - x*b0);
results.beta = b0;
results.rho = rho;
results.lam = lam;
results.resid = e;
results.yhat = y-e;
sigu = e'*e;
sige = sigu/NT1;
results.sige = sige;
pout=[results.beta
      results.rho
      results.lam
      results.sige];
results.lik=f2_sarar_panel(pout,y,x,W1,W2,det1,det2,T);

W1t=kron(It,W1);
W2t=kron(It,W2);

if (N<= Nhes)
% find asymptotic t-stats (from Anselin, 1982, pages 183-184)
bhat = results.beta;
xpx=zeros(nvar+3,nvar+3);
Bi=inv(B);%NxN matrix
Ai=inv(A);%NxN Matrix
Bti=kron(It,Bi);
Ati=kron(It,Ai);
% beta, beta
xpx(1:nvar,1:nvar)=(1/sige)*x'*Bt'*Bt*x;
%rho, rho
term1 = T*trace(Ai*W1*Ai*W1);
term2=(1/sige)*(Bt*W1t*Ati*x*bhat)'*(Bt*W1t*Ati*x*bhat);
term3=T*trace((B*W1*Ai*Bi)'*(B*W1*Ai*Bi));
xpx(nvar+1,nvar+1)=term1+term2+term3;
%lambda, lambda
term1=T*trace(Bi*W2*Bi*W2);
term2=T*trace((W2*Bi)'*(W2*Bi));
xpx(nvar+2,nvar+2)=term1+term2;
%sigma2, sigma2
xpx(nvar+3,nvar+3)=NT1/(2*sige*sige);
%off-diagonal terms
%beta, rho
xpx(1:nvar,nvar+1)=(1/sige)*x'*Bt'*Bt*W1t*Ati*x*bhat;
xpx(nvar+1,1:nvar)=xpx(1:nvar,nvar+1)';
% rho, lambda
term1=T*trace((W2*Bi)'*(B*W1*Ai*Bi));
term2=T*trace(W2*W1*Ai*Bi);
xpx(nvar+1,nvar+2)=term1+term2;
xpx(nvar+2,nvar+1)=xpx(nvar+1,nvar+2)';
% sigma^2, lambda
xpx(nvar+3,nvar+2)=(1/sige)*T*trace(W2*Bi);
xpx(nvar+2,nvar+3)=xpx(nvar+3,nvar+2);
%sigma, rho
xpx(nvar+3,nvar+1)=(1/sige)*T*trace(W1*Ai);
xpx(nvar+1,nvar+3)=xpx(nvar+3,nvar+1);
xpxi = invpd(xpx);
results.cov=xpxi(1:nvar+2,1:nvar+2);
tmp = diag(abs(xpxi));
bvec = [results.beta
        results.rho
        results.lam];
results.tstat = bvec./sqrt(tmp(1:nvar+2,1));

elseif (N > Nhes) % use numerical hessian
t0 = clock;

parm = [results.beta
        results.rho
        results.lam
        results.sige];

hessn = hessian('f2_sarar_panel',parm,y,x,W1,W2,det1,det2,T);
xpxi = invpd(-hessn);
cov=xpxi(1:nvar+2,1:nvar+2);
for i=1:nvar+2
    cov(i,i)=abs(cov(i,i));
end
results.cov=cov;
xpxi = diag(cov);
tmp = [results.beta
       results.rho
       results.lam];
results.tstat = tmp./sqrt(xpxi);
time3 = etime(clock,t0);
end; 
    
results.time = etime(clock,timet);
results.lflag = ldetflag;
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.time4 = time4;


