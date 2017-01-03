function results = sar_panel_FE_LY(y,x,W,N,info)
% PURPOSE: computes spatial lag model estimates for spatial panels (N regions*T time periods)

%           y = p*W*y + X*b + e, using sparse matrix algorithms

% This program has been developed only for individual effects (no time
% effects)
% This function uses the data transformation proposed by Lung-Fei Lee and 
% Jihai Yu (Journal of Econometrics, 2010, 154, 165-185).

% Input of the function should be original data. The transformation is 
% implemented just below. 

% Supply data sorted first by time and then by spatial units, so first region 1,
% region 2, et cetera, in the first year, then region 1, region 2, et
% cetera in the second year, and so on.

% ---------------------------------------------------
%  USAGE: results = sar_panel_FE_LY(y,x,W,T,info)
%  where:  y = dependent variable vector
%          x = independent variables matrix (WITHOUT CONSTANT)
%          W = spatial weights matrix (standardized)
%          N = number of individuals
%       info       = an (optional) structure variable with input options:      
%       info.rmin  = (optional) minimum value of rho to use in search  
%       info.rmax  = (optional) maximum value of rho to use in search 
%       info.convg = (optional) convergence criterion (default = 1e-8)
%       info.maxit = (optional) maximum # of iterations (default = 500)
%       info.lflag = 0 for full lndet computation (default = 1, fastest)
%                  = 1 for MC lndet approximation (fast for very large problems)
%                  = 2 for Spline lndet approximation (medium speed)
%       info.order = order to use with info.lflag = 1 option (default = 50)
%       info.iter  = iterations to use with info.lflag = 1 option (default = 30)  
%       info.Nhes  = Threshold value under which asymptotic variance matrix is computed using analytical formulas,
%                    N > Nhes asymptotic variance matrix is computed using numerical formulas
%                    (Default NHes=500)
% ---------------------------------------------------
%  RETURNS: a structure 
%         results.meth  = sar_panel_FE_LY
%         results.beta  = bhat
%         results.rho   = rho (p above)
%         results.tstat = asymp t-stat (last entry is rho=spatial autoregressive coefficient)
%         results.yhat  = yhat = [inv(y-p*W)]*x*b
%         results.resid = residuals = y-p*W*y-x*b
%         results.sige  = sige = (y-p*W*y-x*b)'*(y-p*W*y-x*b)/N(T-1)
%         results.corr2 = Square of the correlation coefficient (measure of goodness of fit)           
%         results.lik   = log likelihood
%         results.nvar  = # of explanatory variables in x 
%         results.cov   = Variance-covariance matrix of beta and rho
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
% --------------------------------------------------
%  NOTES: if you use lflag = 1 or 2, info.rmin will be set = -1 
%                                    info.rmax will be set = 1
%         For number of spatial units < 500 you should use lflag = 0 to get exact results                                    
% ---------------------------------------------------
%
% written by: J.Paul Elhorst summer 2008
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@eco.rug.nl

% Function modified by N. Debarsy and C. Ertur (fall 2009) to implement the
% Lee and Yu methodology developed in Journal of Econometrics, 2009, 154:2,
% 165-185

% REFERENCES: 
% Elhorst JP (2009) Spatial Panel Data Models. In Fischer MM, Getis A (Eds.) 
% Handbook of Applied Spatial Analysis, Ch. C.2. Springer: Berlin Heidelberg New York.

% Lee, Lung-Fei and Yu, Jihai, (2010), Estimation of spatial autoregressive
% panel data models with fixed effects, Journal of Econometrics, 154:2,
% 165-185


results.meth = 'sar_panel_FE_LY';

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
options = optimset('fminbnd');

if nargin == 5
 if ~isstruct(info)
 error('sar: must supply the options as a structure variable');
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
 error('Wrong # of arguments to sar'); 
end; 


% check size of user inputs for comformability
[nobs nvar] = size(x);
T=nobs/N;
[N Ncol] = size(W);
if N ~= Ncol
error('sar: wrong size weight matrix W');
elseif T ~= nobs/N
error('sar: wrong size weight matrix W or matrix x');
end;
[nchk junk] = size(y);
if nchk ~= nobs
error('sar: wrong size vector y or matrix x');
end;
% Transformation of the data according to the Lee and Yu methodology
% ((Journal of Econometrics, 2009)
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
T=size(y,1)/N; % Effectively T=T-1 since the Lee transformation shrinks the sample from one period of time 
NT1=size(y,1);
% parse input options
[rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,miter,options] = sar_parse(info); % function of LeSage

% compute eigenvalues or limits
[rmin,rmax,time2] = sar_eigs(eflag,W,rmin,rmax,N); % function of LeSage

% do log-det calculations
[detval,time1] = sar_lndet(ldetflag,W,rmin,rmax,detval,order,miter); % function of LeSage

% step 1) do regressions

t0 = clock;

            It=eye(T);
            Wk= kron(It,W);
            wy=Wk*y;
          AI = x'*x;
          b0 = AI\(x'*y);
          bd = AI\(x'*wy);
          e0 = y - x*b0;
          ed = wy - x*bd;
          epe0 = e0'*e0;
          eped = ed'*ed;
          epe0d = ed'*e0;


% step 2) maximize concentrated likelihood function;
	
    [p,liktmp,exitflag,output] = fminbnd('f_sarpanel',rmin,rmax,options,detval,epe0,eped,epe0d,N,T);
   
time4 = etime(clock,t0);

if exitflag == 0
fprintf(1,'sar: convergence not obtained in %4d iterations \n',output.iterations);
end;
results.iter = 1;

% step 3) find b,sige maximum likelihood estimates
results.beta = b0 - p*bd; 
results.rho = p; 
bhat = results.beta;
results.sige = (1/NT1)*(e0-p*ed)'*(e0-p*ed); 
sige = results.sige;
Sn=IN-p*W;
Snk=kron(It,Sn);
results.resid = Snk*y -  x*bhat; 


parm = [results.beta
        results.rho
        results.sige];

results.lik = f2_sarpanel(parm,y,x,W,detval,T); %Elhorst
% Goodness of fit measure
 %Computation of the square of the correlation coefficient 
yhat=zeros(NT1,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    yhat(t1:t2,1)=(speye(N) - p*W)\x(t1:t2,:)*results.beta;
end
results.yhat=yhat;
res1=y-mean(y);
res2=yhat-mean(y);
rsq1=res1'*res2;
rsq2=res1'*res1;
rsq3=res2'*res2;
results.corr2=rsq1^2/(rsq2*rsq3); %corr2


if N <= Nhes
t0 = clock;
% asymptotic t-stats based on information matrix (page 80-81 Anselin, 1980),
% adjusted by Elhorst for spatial panels 

BI = inv(Sn); WB = W*BI;
pterm = trace(WB*WB + WB'*WB);
xpx = zeros(nvar+2,nvar+2);               
% bhat,bhat
xpx(1:nvar,1:nvar) = (1/sige)*(x'*x);     
% bhat,rho
ysum=zeros(nvar,1);
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    ysum=ysum+(1/sige)*x([t1:t2],:)'*WB*x([t1:t2],:)*bhat;
end
xpx(1:nvar,nvar+1) = ysum;
xpx(nvar+1,1:nvar) = xpx(1:nvar,nvar+1)'; 
% rho,rho
ysom=(1/sige)*bhat'*x'*kron(It,BI)'*Wk'*Wk*kron(It,BI)*x*bhat+ T*pterm;
xpx(nvar+1,nvar+1) = ysom;
% sige, sige
xpx(nvar+2,nvar+2) = NT1/(2*sige*sige);     
% rho,sige
xpx(nvar+1,nvar+2) = (T/sige)*trace(WB);  
xpx(nvar+2,nvar+1) = xpx(nvar+1,nvar+2);
xpxi = xpx\eye(size(xpx));
results.cov=xpxi(1:nvar+1,1:nvar+1);
tmp = diag(xpxi(1:nvar+1,1:nvar+1));
results.var=tmp; % New line by Debarsy and Ertur
bvec = [results.beta
        results.rho];
tmp = bvec./(sqrt(tmp));
results.tstat = tmp;
time3 = etime(clock,t0);

else  % asymptotic t-stats using numerical hessian
    
t0 = clock;

dhessn = hessian('f2_sarpanel',parm,y,x,W,detval,T); %Elhorst
xpxi = invpd(-dhessn);
cov=xpxi(1:nvar+1,1:nvar+1);
for i=1:nvar+1
    cov(i,i)=abs(cov(i,i));
end
results.cov=cov;
xpxi = diag(cov);

tmp = [results.beta
       results.rho];
results.tstat = tmp./sqrt(xpxi);
time3 = etime(clock,t0);

end; % end of t-stat calculations

% return stuff
results.y = y;
results.nvar = nvar;
results.rmax = rmax;      
results.rmin = rmin;
results.lflag = ldetflag;
results.order = order;
results.miter = miter;
results.time = etime(clock,timet);
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.time4 = time4;
results.lndet = detval;
results.N = N;
results.T = T;


function [rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,iter,options] = sar_parse(info)
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

fields = fieldnames(info);
nf = length(fields);
if nf > 0
    
 for i=1:nf
    if strcmp(fields{i},'rmin')
        rmin = info.rmin;  eflag = 0;
    elseif strcmp(fields{i},'rmax')
        rmax = info.rmax; eflag = 0;
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
    end;
 end;
 
else % the user has input a blank info structure
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
