function result = mess(y,x,options)
% PURPOSE: maximum likelihood MESS estimates
% S*y = X*b + e, S = e^aD, with xflag == 0, or:
% S*y = [i X D*X]*b + e, xflag == 1
% D = a row-stochastic spatial weight matrix on input 
% or: a weight matrix constructed from neighbors
% NOTE: with options.rho and options.neigh 
% the weight matrix is constructed for you as:
% D = sum rho^i N_i / sum rho^i, i=1,...,#neighbors
% with i = # neighbors, N_i a matrix with ith nearest neighbors
% ------------------------------------------------
% Usage: results = mess(y,x,options)
% where: y = dependent variable vector (nx1 vector)
%        x = explanatory variables matrix (nxk matrix)
%  options = a structure variable with:
%  options.D     = spatial weight matrix (nxn matrix)
%  options.latt  = lattitude coordinates (nx1 vector)
%  options.long  = longitude coordinates (nx1 vector)
%  options.neigh = # of neighbors to use with latt,long options
%                  in constructing D (default = 5)
%  options.xflag = 0 for S*y = X*b + e,         model (default)
%                = 1 for S*y = [i X D*X]*b + e, model
%  options.rho   = value of rho to use in discounting 
%                  (0 < rho < 1), default = 1, no discounting
%  options.hflag = 0 or 1, to compute std deviations of
%                  the estimates using a numerical hessian
%                 (default = 1, compute the hessian) 
%  options.nflag = 0 for neighbors using 1st and 2nd order Delauney (default)
%                = 1 for neighbors using 3rd and 4th order Delauney
%                  for large nobs, and large # neighbors, used nflag = 1
%  options.q     = # of terms to use in the matrix exponential
%                  expansion (default = 7)
% ------------------------------------------------
% RETURNS:
% results.bhat  = beta estimates (nvar x 1)
% results.bstd  = std of beta estimates (nvar x 1)
% results.tstat = t-statistics (nvar+1 x 1), last is alpha t-stat
% results.alpha = alpha estimate
% results.astd  = std of alpha estimate
% results.sige  = sigma estimate
% results.rsqr  = rsquared
% results.rbar  = adjusted rsquared
% results.y     = y vector from input
% results.yhat  = predicted values for y
% results.resid = residuals
% results.nobs  = # of observations
% results.nvar  = # of explantory variables in X (plus D*X)
% results.lik   = log likelihood function value
% results.time  = total time (in seconds)
% results.htime = time taken for hessian calculations
% results.ntime = time take for neighbors calculations
% results.xflag = model flag from input
% results.nflag = nflag argument from input
% results.hflag = hflag argument from input
% results.rho   = rho value from input (or default)
% results.neigh = # of terms in flexible D-matrix specification
%                 (from input or default)
% results.q     = q value from input (or default)
% ------------------------------------------------
% NOTES: if the model includes a constant term
% it should be entered as the first column in the x-matrix
% that is input to the function
% ------------------------------------------------
% REFERENCES: Pace and LeSage (2000) "Closed-Form Maximum
% Likelihood Estimates for Spatial Problems", unpublished
% manuscript
% ------------------------------------------------
% SEE ALSO: mess_g, prt
% ------------------------------------------------

% written by:
% James P. LeSage, 7/2000
% Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial.econometrics.com
% (With a great deal of help from R. Kelley Pace, 
% the mastermind on this one)

% -----------------------------------
% set up defaults
q = 7;
xflag = 0;
nflag = 0;
hflag = 1;
rho = 1;
neigh = 5;
dflag = 1;
% ------------------------------------

% parse options input and do error checking
[n junk] = size(y);
results.y = y;
[n1 k] = size(x);

if n1 ~= n
error('mess: x-matrix contains wrong # of observations');
end;

if nargin == 3
fields = fieldnames(options);
 nf = length(fields);
 for i=1:nf
    if strcmp(fields{i},'xflag')
       xflag = options.xflag;
    elseif strcmp(fields{i},'rho')
        rho = options.rho;
    elseif strcmp(fields{i},'q')
       q = options.q;
     elseif strcmp(fields{i},'D')
        D = options.D;
        dflag = 0; % a flag to detect user weight matrix options
     elseif strcmp(fields{i},'neigh')
       neigh = options.neigh;
     elseif strcmp(fields{i},'latt')
        latt = options.latt;
        dflag = dflag + 1; % a flag to detect user weight matrix options
     elseif strcmp(fields{i},'long')
        long = options.long;
        dflag = dflag + 1;
     elseif strcmp(fields{i},'hflag')
        hflag = options.hflag;
     elseif strcmp(fields{i},'nflag')
        nflag = options.nflag;
    end;
 end;

else
error('Wrong # of arguments to mess');
end;

% error checking on D versus latt,long
 if dflag == 0
   [n1 n2] = size(D);
   if n1 ~= n2
      error('mess: Wrong size D matrix on input');
   end;
   if n1 ~= n
      error('mess: Wrong size D matrix on input');
   end;
elseif dflag == 1
      error('mess: either D or latt,long must be input');
 elseif dflag == 2
      error('mess: either latt or long missing on input');
 elseif dflag == 3
      [n1 n2] = size(latt);
      [n3 n4] = size(long);
      if n1 ~= n3
         error('mess: latt and long wrong on input');
      end;
      if n2 ~= 1 
         error('mess: latt and long should be vectors');
      end;
      if n4 ~= 1
            error('mess: latt and long should be vectors');
      end;
 end;
      

t0 = clock; % start timer
htime = 0;
ntime = 0;

switch xflag % switch on x transformation
   
case{0} % case where x variables are not transformed
   
tmp = rho.^(0:neigh-1);
tmp = tmp/sum(tmp);

if dflag == 3 % we have to construct the weight matrix
              % using neighbors
              % construct nearest neighbors weight matrix
 t1 = clock;  % time this operation

 % construct ymat, spatially transformed y
 % find index into nearest neighbors
 if nflag == 0
 nnlist = find_nn(latt,long,neigh);
 elseif nflag == 1
 nnlist = find_nn2(latt,long,neigh);
 else 
 error('mess: bad option.nflag input argument');
 end;

% check for empty nnlist columns
chk = find(nnlist == 0);
if length(chk) > 0; 
 if nflag == 1 % no saving the user here
 error('mess: trying too many neighbors, some do not exist');
 else % we save the user here
 nnlist = find_nn2(latt,long,neigh);
 end;
end;


 wy = y;
 ymat = y(:,ones(1,q));
 for i=2:q;
 wy = wy(nnlist)*tmp';
 ymat(:,i) = wy;
 end;
 ntime = etime(clock,t1);

elseif dflag == 0 % we may need to apply discounting
                  % to the user's D matrix
 DS = D;
 wiy = y;
 ymat = y;
 for i=1:q;
 wiy = DS*wiy;
 ymat = [ymat wiy];
 end;
neigh = 0;
rho = 0;
end; % end of constructing Sy stuff

 
bmat = (x'*x)\(x'*ymat);  
emat=ymat-x*bmat;
ssemat=emat'*emat;
% nq is q in the paper
nq=length(ssemat);
nq1=nq-1;
% rrr is the diag(W) in the paper
diagrrr = diag((1./[1 cumprod(1:nq1)]));
zzz=diagrrr*ssemat*diagrrr;
% the following extracts coefficients of the polynomial
% using antidiagonal traces
flipzzz=flipud(zzz);
p=fliplr(sum(spdiags(flipzzz,-nq1:nq1)));
% solve for mess pstar
posstar=roots(polyder(p));
realind=imag(posstar)==0;
pstar=posstar(realind);
pstarvec=(pstar.^(0:nq1))'; % optimal vector
qstar=pstarvec'*zzz*pstarvec;
Sy = ymat*diagrrr*pstarvec;
% ------------------

bhat = bmat*diagrrr*pstarvec; % mess bhat
e = Sy - x*bhat; % mess residuals
epeml = qstar;
yhat = y - e;
sigu = epeml;
sige = sigu/(n-k);
llike = -(n/2)*log(qstar);
ym = y - mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(n-k);
rsqr2 = rsqr2/(n-1.0);
rbar = 1 - (rsqr1/rsqr2); % rbar-squared

if hflag == 1
% use numerical hessian to get var-cov matrix estimate
parm = [bhat
        pstar
        sige];
t1 = clock;     
hessn = hessian('mess_like',parm,y,x,ymat);
htime = etime(clock,t1);
xpxi = invpd(hessn);
[n k] = size(x);     
xpxi = diag(xpxi(1:k+1,1:k+1));
tmp = [bhat
       pstar];
result.tstat = tmp./sqrt(xpxi);

bstd = sqrt(xpxi(1:k,1));
astd = sqrt(xpxi(k+1,1)); 
else
   tstat = zeros(k+1,1);
   bstd = zeros(k,1);
   astd = 0;
end; % end of if hflag == 1

time = etime(clock,t0);
     
result.meth = 'mess';
result.y = y;
result.nobs = n;
result.nvar = k;
result.rsqr = rsqr;
result.rbar = rbar;
result.yhat = yhat;
result.sige = sige;
result.bhat = bhat;
result.bstd = bstd;
result.alpha = pstar;
result.astd  = astd;
result.resid = e;
result.lik   = llike;
result.time  = time;
result.htime = htime;
result.xflag = xflag;
result.nflag = nflag;
result.hflag = hflag;
result.rho   = rho;
result.neigh = neigh;
result.q     = q;
result.ntime = ntime;

case{1} % case of x variables spatially transformed
   
   xone = x(:,1);
   if all(xone == 1)
      xsub = x(:,2:k);
   else
      xsub = x;
   end;
   
tmp = rho.^(0:neigh-1);
tmp = tmp/sum(tmp);

if dflag == 3 % we have to construct the weight matrix
              % using neighbors
 t1 = clock;  % time neighbor formulation       

  % construct ymat, xmat spatially transformed y, x
 % find index into nearest neighbors
 if nflag == 0
 nnlist = find_nn(latt,long,neigh);
 elseif nflag == 1
 nnlist = find_nn2(latt,long,neigh);
 else 
 error('mess: bad option.nflag input argument');
 end;

% check for empty nnlist columns
chk = find(nnlist == 0);
if length(chk) > 0; 
 if nflag == 1 % no saving the user here
 error('mess: trying too many neighbors, some do not exist');
 else % we save the user here
 nnlist = find_nn2(latt,long,neigh);
 end;
end;


 wy = y;
 ymat = y(:,ones(1,q));
 for i=2:q;
 wy = wy(nnlist)*tmp';
 ymat(:,i) = wy;
 end;

[junk nk] = size(xsub);
xmat = x;
for i=1:nk;
xi = xsub(:,i);
tmpp = xi(nnlist)*tmp';
xmat = [xmat tmpp];
end;

 ntime = etime(clock,t1);

elseif dflag == 0 % we may need to apply discounting
                  % to the user's D matrix
 t1 = clock;  % time neighbor formulation       

DS = D;
wiy = y;
ymat = y;
for i=1:q;
wiy = DS*wiy;
ymat = [ymat wiy];
end;
xmat = [x DS*xsub];
neigh = 0;
rho = 0;
 ntime = etime(clock,t1);

end; % end of constructing xmat, ymat matrix stuff

bmat = xmat\ymat;  
emat=ymat-xmat*bmat;
ssemat=emat'*emat;

% nq is q in the paper
nq=length(ssemat);
nq1=nq-1;
% rrr is the diag(W) in the paper
diagrrr = diag((1./[1 cumprod(1:nq1)]));
zzz=diagrrr*ssemat*diagrrr;
% the following extracts coefficients of the polynomial
% using antidiagonal traces
flipzzz=flipud(zzz);
p=fliplr(sum(spdiags(flipzzz,-nq1:nq1)));
% solve for mess pstar
posstar=roots(polyder(p));
realind=imag(posstar)==0;
pstar=posstar(realind);
pstarvec=(pstar.^(0:nq1))'; % optimal vector
qstar=pstarvec'*zzz*pstarvec;
Sy = ymat*diagrrr*pstarvec;

bhat = bmat*diagrrr*pstarvec;

e = Sy - xmat*bhat;
epeml = qstar;
yhat = y - e;
sige = epeml/(n-k);

llike = -(n/2)*log(qstar);

ym = y - mean(y);
rsqr1 = epeml;
rsqr2 = ym'*ym;
rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(n-k);
rsqr2 = rsqr2/(n-1.0);
rbar = 1 - (rsqr1/rsqr2); % rbar-squared

if hflag == 1
% use numerical hessian to get var-cov matrix estimate
parm = [bhat
        pstar
        sige];
t1 = clock;     
hessn = hessian('mess_like',parm,y,xmat,ymat);
htime = etime(clock,t1);
xpxi = invpd(hessn);
[junk kk] = size(xmat);    
xpxi = diag(xpxi(1:kk+1,1:kk+1));
tmp = [bhat
       pstar];
result.tstat = tmp./sqrt(xpxi);


bstd = sqrt(xpxi(1:kk,1));
astd = sqrt(xpxi(kk+1,1));    
else
   [junk kk] = size(xmat);    
   tstat = zeros(kk+1,1);
   bstd = zeros(kk,1);
   astd = 0;
end; % end of if hflag == 1


time = etime(clock,t0);

result.meth = 'mess';
result.y = y;
result.nobs = n;
result.nvar = k;
result.rsqr = rsqr;
result.rbar = rbar;
result.yhat = yhat;
result.sige = sige;
result.bhat = bhat;
result.bstd = bstd;
result.alpha = pstar;
result.astd  = astd;
result.resid = e;
result.lik   = llike;
result.time  = time;
result.htime = htime;
result.ntime = ntime;
result.xflag = xflag;
result.nflag = nflag;
result.hflag = hflag;
result.rho   = rho;
result.neigh = neigh;
result.q     = q;
   
otherwise
   
end; % end of switch

