function results = sem_panel_FE_LY(y,x,W,N,info)
% PURPOSE: computes spatial error model estimates for spatial panels (N regions*T time periods)

%           y = XB + u,  u = p*W*u + e, using sparse algorithms 

% This program has been developed only for individual effects (no time
% effects)
% This function uses the data transformation proposed by Lung-Fei Lee and 
% Jihai Yu (Journal of Econometrics, 2010, 154, 165-185).

% Input of the function should be original data. The transformation is 
% implemented just below. 

% Supply data sorted first by time and then by spatial units, so first region 1,
% region 2, et cetera, in the first year, then region 1, region 2, et
% cetera in the second year, and so on

% ---------------------------------------------------
%  USAGE: results = sem_panel_FE_LY(y,x,W,T,info)
%  where: y = dependent variable vector
%         x = independent variables matrix (WITHOUT CONSTANT) 
%         W = spatial weights matrix (standardized)
%         N = number of individuals
%       info       = an (optional) structure variable with input options:
%       info.rmin  = (optional) minimum value of rho to use in search  
%       info.rmax  = (optional) maximum value of rho to use in search    
%       info.convg = (optional) convergence criterion (default = 1e-4)
%       info.maxit = (optional) maximum # of iterations (default = 500)
%       info.parm  = (optional) initial value for rho (default = 0.5).
%       info.lflag = 0 for full lndet computation (default = 1, fastest)
%                  = 1 for MC lndet approximation (fast for very large problems)
%                  = 2 for Spline lndet approximation (medium speed)
%       info.order = order to use with info.lflag = 1 option (default = 50)
%       info.iter  = iterations to use with info.lflag = 1 option (default = 30)     
%       info.Nhes  = Threshold value under which asymptotic variance matrix is computed using analytical formulas,
%                    N > Nhes asymptotic variance matrix is computed using numerical formulas
%                    (Default NHes=500))
% ---------------------------------------------------
%  RETURNS: a structure  
%         results.meth  = sem_panel_FE_LY
%         results.beta  = bhat
%         results.rho   = rho (p above)
%         results.tstat = asymp t-stats (last entry is rho=spatial autocorrelation coefficient)
%         results.yhat  = yhat
%         results.resid = residuals
%         results.sige  = sige = e'(I-p*W)'*(I-p*W)*e/nobs
%         results.corr2 = Square of the correlation coefficient (measure of goodness of fit)           
%         results.lik   = log likelihood
%         results.nvar  = # of explanatory variables in x
%         results.cov   = Variance-covariance matrix of beta and rho
%         results.cov2  = Variance-covariance matrix of beta, rho and
%                         sigma2
%         results.y     = y data vector
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
%         results.N     = # of individuals considered
%         results.T     = # of periods considered (after the Lee and Yu
%                         transformation (=T-1)
%         results.lndet = a matrix containing log-determinant information
%                          (for use in later function calls to save time)
%  --------------------------------------------------
%  NOTES: if you use lflag = 1 or 2, info.rmin will be set = -1 
%                                    info.rmax will be set = 1
%         For number of spatial units < 500 you should use lflag = 0 to get exact results
% ---------------------------------------------------
%
% Updated by: J.Paul Elhorst summer 2008
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@eco.rug.nl
%
%  Function modified by N. Debarsy* and C. Ertur** (fall 2009) to implement the
% Lee and Yu methodology developed in Journal of Econometrics, 2009, 154:2,
% 165-185

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

% REFERENCES: 
% Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.

% Lee, Lung-Fei and Yu, Jihai, (2010), Estimation of spatial autoregressive
% panel data models with fixed effects, Journal of Econometrics, 154:2,
% 165-185

% This function is based on James. P LeSage's function SEM

time1 = 0; 
time2 = 0;
time3 = 0;
time4 = 0;
timet = clock; % start the clock for overall timing

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
rmin = -0.99;
rmax = 0.99;
parm= 0.5;

options = optimset('fminbnd');
if nargin == 5
 if ~isstruct(info)
 error('sem: must supply the options as a structure variable');
 end;
 fields = fieldnames(info);
 nf = length(fields);
    for i=1:nf
        if strcmp(fields{i},'convg')
        options.TolFun = info.convg;
        elseif strcmp(fields{i},'maxit')
        options.MaxIter  = info.maxit;
        elseif strcmp(fields{i},'rmin')
        rmin = info.rmin;
        elseif strcmp(fields{i},'rmax')
        rmax = info.rmax;
        elseif strcmp(fields{i},'parm')
        parm = info.parm;
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
elseif nargin == 4 % use default options
options = optimset('fminbnd');
else
 error('Wrong # of arguments to sem'); 
end; 



% check size of user inputs for comformability
[nobs nvar] = size(x);
[N Ncol] = size(W);
T=nobs/N;
if N ~= Ncol
error('sem: wrong size weight matrix W');
elseif T ~= nobs/N
error('sem: wrong size weight matrix W or matrix x');
end;
[nchk junk] = size(y);
if nchk ~= nobs
error('sem: wrong size vector y or matrix x');
end;

results.y = y;
results.nvar = nvar; 
results.meth='sem_panel_FE_LY';

% parse input options
[rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,miter,options] = sem_parse(info); %function of LeSage

% compute eigenvalues or limits
[rmin,rmax,time2] = sem_eigs(eflag,W,rmin,rmax,N); %function of LeSage

results.rmin = rmin;
results.rmax = rmax;
results.lflag = ldetflag;
results.miter = miter;
results.order = order;

% do log-det calculations
[detval,time1] = sem_lndet(ldetflag,W,rmin,rmax,detval,order,miter); % function of LeSage

% Transformation of the data according to the Lee and Yu methodology (Journal of Econometrics, 2009)
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
T=size(y,1)/N; % Effectively T=T-1 since the Lee transformation shrinks the sample from NT to N(T-1). 
NT1=size(y,1);
results.ytrans=y;
results.xtrans=x;
t0 = clock;

for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    Wx([t1:t2],:)= sparse(W)*x([t1:t2],:);
    Wy([t1:t2],1)= sparse(W)*y([t1:t2],1);
end

rho = parm;
converge = 1;
criteria = 1e-4;
iter = 1;

% Two-stage iterative procedure to find the ML estimates
while (converge > criteria) & (iter < maxit)

xs = x - rho*Wx;
ys = y - rho*Wy;
b = (xs'*xs)\(xs'*ys);
e = (y - x*b);
rold = rho;
[rho,like,exitflag,output] = fminbnd('f_sempanel',rmin,rmax,options,e,W,detval,T);
converge = abs(rold - rho);
iter = iter + 1;
end;


liktmp = like;
time4 = etime(clock,t0);

if exitflag == maxit
fprintf(1,'\n sem: convergence not obtained in %4d iterations \n',output.iterations);
end;
results.iter = output.iterations;

% Results 
% We compute a last iteration of the parameter beta to include the last version of rho.
xs = x - rho*Wx;
ys = y - rho*Wy;
b = (xs'*xs)\(xs'*ys);
e = (y - x*b);
results.beta= b;
results.rho = rho;
results.resid=e;
B = (speye(N) - rho*sparse(W));
Be=zeros(N*T,1);

for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    Be([t1:t2],1)= B*results.resid([t1:t2],1);
end
epe = Be'*Be;
 results.sige = (1/(NT1))*epe;
% Goodness of fit measure
 %Computation of the square of the correlation coefficient 
yhat=x*results.beta;
res1=y-mean(y);
res2=yhat-mean(y);
rsq1=res1'*res2;
rsq2=res1'*res1;
rsq3=res2'*res2;
results.corr2=rsq1^2/(rsq2*rsq3); %corr2

sige = results.sige;
parm = [results.beta
        results.rho
        results.sige];

results.lik = f2_sempanel(parm,y,x,W,detval,T); %Elhorst

% Determination variance-covariance matrix
if N <= Nhes, 
t0 = clock;

BI = inv(B); WB = W*BI;
pterm = trace(WB'*WB);
xpx = zeros(nvar+2,nvar+2);
%bhat, bhat
xpx(1:nvar,1:nvar) = (1/sige)*xs'*xs;
% rho, rho
xpx(nvar+1,nvar+1) = T*(trace(WB*WB) + pterm);
% sige, sige
xpx(nvar+2,nvar+2) = (N*T)/(2*sige*sige);
% rho, sige
xpx(nvar+1,nvar+2) = (T/sige)*trace(WB);
xpx(nvar+2,nvar+1) = xpx(nvar+1,nvar+2);
xpxi=xpx\eye(size(xpx));
results.cov=xpxi(1:nvar+1,1:nvar+1);
results.cov2=xpxi;
tmp = diag(xpxi);
bvec = [results.beta
        results.rho];
results.tstat = bvec./(sqrt(tmp(1:nvar+1,1)));
time3 = etime(clock,t0);


elseif N > Nhes

t0 = clock;
hessn = hessian('f2_sempanel',parm,y,x,W,detval,T); %Elhorst

if hessn(nvar+2,nvar+2) == 0
 hessn(nvar+2,nvar+2) = 1/sige;  % this is a hack for very large models that 
end;                             % should not affect inference in these cases


xpxi = invpd(-hessn);
cov=xpxi(1:nvar+1,1:nvar+1);
for i=1:nvar+1
    cov(i,i)=abs(cov(i,i));
end
results.cov=cov;
xpxi = diag(cov);
 bvec = [results.beta
        results.rho];
 results.tstat = bvec./sqrt(xpxi);
 
time3 = etime(clock,t0);

end; % end of t-stat calculations

results.lndet = detval;
results.time = etime(clock,timet);
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.time4 = time4;
results.N     = N;
results.T     = T;



function [rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,iter,options] = sem_parse(info)
% PURPOSE: parses input arguments for far, far_g models
% ---------------------------------------------------
%  USAGE: [rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,iter] = far_parse(info)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% set defaults
options = optimset('fminbnd');
options.MaxIter = 500;

eflag = 1;     % default to not computing eigenvalues
ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
order = 50;    % there are parameters used by the MC det approx
iter = 30;     % defaults based on Pace and Barry recommendation
rmin = -0.99;     % use -1,1 rho interval as default
rmax = 0.99;
detval = 0;    % just a flag
convg = 0.0001;
maxit = 500;

fields = fieldnames(info);
nf = length(fields);
if nf > 0
    
 for i=1:nf
    if strcmp(fields{i},'rmin')
        rmin = info.rmin;  eflag = 1;
    elseif strcmp(fields{i},'rmax')
        rmax = info.rmax;  eflag = 1;
    elseif strcmp(fields{i},'eigs')
        eflag = info.eigs; % flag for compute the eigenvalues
    elseif strcmp(fields{i},'convg')
       options.TolFun = info.convg;
    elseif strcmp(fields{i},'maxit')
        options.MaxIter = info.maxit;  
    elseif strcmp(fields{i},'lndet')
    detval = info.lndet;
    ldetflag = -1;
    eflag = 1;
    rmin = detval(1,1);
    nr = length(detval);
    rmax = detval(nr,1);
    elseif strcmp(fields{i},'lflag')
        tst = info.lflag;
        if tst == 0,
        ldetflag = 0; 
        elseif tst == 1,
        ldetflag = 1; 
        elseif tst == 2,
        ldetflag = 2; 
        else
        error('sar: unrecognizable lflag value on input');
        end;
    elseif strcmp(fields{i},'order')
        order = info.order;  
    elseif strcmp(fields{i},'iter')
        iter = info.iter; 
    end;
 end;
 
else, % the user has input a blank info structure
      % so we use the defaults
end; 

function [rmin,rmax,time2] = sem_eigs(eflag,W,rmin,rmax,n);
% PURPOSE: compute the eigenvalues for the weight matrix
% ---------------------------------------------------
%  USAGE: [rmin,rmax,time2] = far_eigs(eflag,W,rmin,rmax,W)
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


function [detval,time1] = sem_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
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
            error('sem: wrong lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('sem: wrong sized lndet input argument');
        elseif n1 == 1
            error('sem: wrong sized lndet input argument');
        end;          
end;

