function results = far(y,W,info)
% PURPOSE: computes 1st-order spatial autoregressive estimates
%           y = iota + p*W*y + e, using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE: results = far(y,W,info)
%  where:  y = dependent variable vector
%          W = standardized contiguity matrix 
%       info = a structure variable with input options
%       info.rmin = (optional) minimum value of rho to use in search  
%       info.rmax = (optional) maximum value of rho to use in search    
%       info.convg = (optional) convergence criterion (default = 1e-8)
%       info.maxit = (optional) maximum # of iterations (default = 500)
%       info.lflag = 0 for full computation (default = 1, fastest)
%                  = 1 for Pace and Barry 1999 MC approximation (fast for very large problems)
%                  = 2 for Pace and Barry 1998 Spline approximation (medium speed)
%       info.order = order to use with info.lflag = 1 option (default = 50)
%       info.iter  = iterations to use with info.lflag = 1 option (default = 30)     
%       info.lndet = a matrix returned by sar, sar_g, sarp_g, etc.
%                    containing log-determinant information to save time
% ---------------------------------------------------
%  RETURNS: a structure
%         results.meth  = 'far'
%         results.rho   = rho
%         results.tstat = asymptotic t-stat
%         results.pstd  = std deviation of rho
%         results.yhat  = yhat
%         results.resid = residuals
%         results.sige  = sige = (y-p*W*y)'*(y-p*W*y)/n
%         results.rsqr  = rsquared
%         results.lik   = log likelihood
%         results.nobs  = nobs
%         results.nvar  = nvar = 1 
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
%         results.lndet = a matrix containing log-determinant information
%                          (for use in later function calls to save time)
% --------------------------------------------------
%  NOTES: if you use lflag = 1 or 2, info.rmin will be set = -1 
%                                    info.rmax will be set = 1
%         For n < 1000 you should use lflag = 0 to get exact results                                                                        
% --------------------------------------------------  
%  SEE ALSO: prt(results), sar, sem, sac, sdm
% ---------------------------------------------------
% REFERENCES: Anselin (1988), pages 180-182.
% Ronald Barry and R. Kelley Pace, "A Monte Carlo Estimator
% of the Log Determinant of Large Sparse Matrices", Linear Algebra and
% its Applications", Volume 289, Number 1-3, 1999, pp. 41-54.
% R. Kelley Pace and Ronald P. Barry "Simulating Mixed Regressive
% Spatially autoregressive Estimators", Computational Statistics, 1998,
% Vol. 13, pp. 397-418.
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics 4/2002
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial.econometrics.com

% NOTE: much of the speed for large problems comes from:
% the use of methods pioneered by Pace and Barry.
% R. Kelley Pace was kind enough to provide functions
% lndetmc, and lndetint from his spatial statistics toolbox
% for which I'm very grateful.

time1 = 0; 
time2 = 0;
time3 = 0;

timet = clock; % start the clock for overall timing

% if we have no options, invoke defaults
if nargin == 2
    info.lflag = 1;
end;
% parse input options
[rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,iter,options] = far_parse(info);

% do some error checking
[n junk] = size(y); 
% test if W-matrix has the correct number of observations
[nchk1 nchk2] = size(W);
if nchk1 ~= n
error('far: Wrong size W-matrix');
end;

% compute eigenvalues or limits
[rmin,rmax,time2] = far_eigs(eflag,W,rmin,rmax,n);

% do log-det calculations
[detval,time1] = far_lndet(ldetflag,W,rmin,rmax,detval,order,iter);


% step 1) maximize concentrated likelihood function;
t0 = clock;
 [p,liktmp,exitflag,output] = fminbnd('f_far',rmin,rmax,options,y,W,detval);
 
time4 = etime(clock,t0);


 if output.iterations == options.MaxIter;
 fprintf(1,'far: convergence not obtained in %4d iterations \n',output.iterations);
 end;
 results.miter = output.iterations;

% step 2) find sige
Wy = sparse(W)*y;
e = (speye(n) - p*W)*y;
yhat = y - e;
epe = e'*e; 
sige = epe/n; 
results.rho = p; 
results.yhat = yhat;
results.resid = e;
results.sige = sige;
results.lik = -(liktmp + (n/2)*log(sige));
parm = [p
        sige];


% asymptotic t-stats 
if n <= 500
% asymptotic t-stats based on information matrix
% (page 50 Anselin, 1980)
t0 = clock;
B = speye(n) - p*sparse(W);  
xpxi = zeros(2,2); 
term1 = trace(inv(B'*B)*(W'*W));  xpxi(1,1) = term1;
xpxi(2,2) = n/(2*sige*sige);   % sige,sige term
xpxi(1,2) = -(1/sige)*(p*term1 - trace(inv(B'*B)*W)); 
xpxi(2,1) = xpxi(1,2);         % sige,rho term
xpxi = invpd(xpxi);
time3 = etime(clock,t0);
results.tstat = results.rho./(sqrt(xpxi(1,1)));
results.ssige = sqrt(xpxi(2,2));
 results.pstd = sqrt(xpxi(1,1));

elseif n > 500 & ldetflag > 0 % use numerical hessian and fast Pace approximation
t0 = clock;
hessn = hessian('f2_far',parm,y,W,detval);
time3 = etime(clock,t0);
if hessn(2,2) == 0
 hessn(2,2) = 1/sige;  % this is a hack for very large
end;                     % spatial autoregressive models
                         % should not affect inference in these cases
xpxi = invpd(hessn);

 results.tstat = results.rho/sqrt(xpxi(1,1));
 results.pstd = sqrt(xpxi(1,1));

elseif n > 500 & ldetflag == 0 % use numerical hessian and slow method
t0 = clock;
hessn = hessian('f2_far',parm,y,W);
time3 = etime(clock,t0);
xpxi = invpd(hessn);
results.tstat = results.rho/sqrt(xpxi(1,1));
results.pstd = sqrt(xpxi(1,1));


end; % end of t-stat calculations

ym = y - mean(y);       % r-squared, rbar-squared
rsqr1 = results.resid'*results.resid;
rsqr2 = ym'*ym;
results.rsqr = 1.0-rsqr1/rsqr2;   % r-squared

results.lndet = detval;

time = etime(clock,timet);

results.meth = 'far';

results.y = y;      
results.nobs = n;
results.nvar = 1;   
results.order = order;
results.iter = iter;

results.rmax = rmax;
results.rmin = rmin;

results.lflag = ldetflag;
results.lndet = detval;


% send back timing information
results.time = time;
results.time1 = time1;
results.time2 = time2;
results.time3 = time3;
results.time4 = time4;


function [rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,iter,options] = far_parse(info)
% PURPOSE: parses input arguments for far, far_g models
% ---------------------------------------------------
%  USAGE: [rmin,rmax,convg,maxit,detval,ldetflag,eflag,order,iter] = far_parse(info)
% where info contains the structure variable with inputs 
% and the outputs are either user-inputs or default values
% ---------------------------------------------------

% set defaults
options = optimset('fminbnd');
options.MaxIter = 500;


eflag = 0;     % default to not computing eigenvalues
ldetflag = 1;  % default to 1999 Pace and Barry MC determinant approx
order = 50;    % there are parameters used by the MC det approx
iter = 30;     % defaults based on Pace and Barry recommendation
rmin = -1;     % use -1,1 rho interval as default
rmax = 1;
detval = 0;    % just a flag
maxit = 500;
convg = 0.0001;

fields = fieldnames(info);
nf = length(fields);
if nf > 0
    
 for i=1:nf
    if strcmp(fields{i},'rmin')
        rmin = info.rmin;  eflag = 1;
    elseif strcmp(fields{i},'rmax')
        rmax = info.rmax;  eflag = 1;
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
        ldetflag = 0; eflag = 0; % compute eigenvalues
        elseif tst == 1,
        ldetflag = 1; eflag = 1; % reset this from default
        elseif tst == 2,
        ldetflag = 2; eflag = 1; % reset this from default
        else
        error('far: unrecognizable lflag value on input');
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

function [rmin,rmax,time2] = far_eigs(eflag,W,rmin,rmax,n);
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


function [detval,time1] = far_lndet(ldetflag,W,rmin,rmax,detval,order,iter);
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
            error('far: wrgon lndet input argument');
        end;
        [n1,n2] = size(detval);
        if n2 ~= 2
            error('far: wrong sized lndet input argument');
        elseif n1 == 1
              error('far: wrong sized lndet input argument');
        end;          
end;

