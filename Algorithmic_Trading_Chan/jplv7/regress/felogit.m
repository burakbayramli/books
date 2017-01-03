function results = felogit(y,x,ivec,tvec,beta0,c0,maxit,tol);
% PURPOSE: computes binomial logistic regression with a one-dimensional fixed effect:
%   logit(y_it) = x_it'beta + c_i
%   where:  i = 1,...,nobs indexes observational units
%           t = 1,...,T_i  indexes repeated observations on i
%         c_i is a fixed effect associated with i
%   NOTE: felogit() returns MLEs of beta AND c_i
%----------------------------------------------------------------%
% USAGE: results = felogit(y,x,i,t,maxit,tol)
% where: y = dependent variable vector (nobs x 1)
%        x = matrix of covariates (nobs x nvar)
%            Note: the model does NOT include a common
%            intercept, instead it includes an effect for 
%            each i.
%     ivec = (nobs x 1) vector which identifies the unit
%            (i) associated with each row of y,x (i.e., the
%            unit for which we have repeated observations).
%            Note: must be numeric
%     tvec = (nobs x 1) vector which identifies the replication
%            number (t) of each row of y,x (i.e., index for
%            repeated observations). Note: must be numeric.
%    beta0 = optional starting values for beta (default is ols)
%       c0 = optional starting values for c (default is zero)
%    maxit = optional maximum number of iterations (default=100)
%      tol = optional convergence criterion (default=1e-8)
%     nobs = sum(T_i) ( = N*T if T_i=T for all i)
%----------------------------------------------------------------%
% RETURNS: a structure
%     results.meth = 'felogit'
%     results.beta = MLE of beta vector                                      
%     results.covb = covariance matrix of results.beta (negative 
%                    inverse of appropriate block of Hessian, H11)
%     results.stdb = standard errors of results.beta
%   results.tstatb = t-stats for results.beta
%        results.c = MLE of fixed effects vector c
%     results.yfit = fitted y-values, i.e., Pr(y_it=1)
%      results.lik = value of log-likelihood
%     results.cnvg = convergence criterion
%     results.iter = number of iterations
%     results.nobs = total number of observations
%     results.nvar = number of covariates
%        results.y = y vector
%        results.N = number of units (i)
%        results.T = maximum number of observations on a single 
%                    unit (i)
%       results.n0 = number of units (i) dropped because y_it=0 
%                    for all t
%       results.n1 = number of units (i) dropped because y_it=1 
%                    for all t
%        results.d = sparse design matrix of estimated fixed effects
%     results.rsqr = McFadden pseudo-R-squared
%  results.lratio1 = LR test-statistic: no slopes, common intercept
%  results.lratio2 = LR test-statistic: no heterogeneity (just 
%                    slopes and common intercept)
%     results.covc = covariance matrix of the estimated fixed effects 
%                    (negative inverse of appropriate block of Hessian, H22)
%     results.stdc = standard errors of estimated fixed effects
%   results.tstatc = t-stats for estimated fixed effects
%    results.covbc = cov(results.beta,results.c), i.e., negative
%                    inverse appropriate block of Hessian, H12
%
%----------------------------------------------------------------%
% Notes:
% 1. Data should be sorted by t within i. A future version of 
% will check that the data are sorted and sort if required.
% 2. felogit uses the functions ols() and logit() for computing 
% starting values and testing, respectively. These functions are
% part of the Econometrics Toolbox by James P. LeSage, and 
% are available at www.spatial-econometrics.com
% 3. felogit() uses an algorithm to compute the MLE of the fixed 
% effects logit model which uses an analytic simplification of the 
% Newton-Raphson update. Thanks to George Jakubson for showing me 
% this computational trick.
% 4. Generally speaking, the felogit has been optimized for solving
% 'moderately large' problems (i.e., nobs = 500,000 and N = 50,000 
% or so). The multiple clear and pack statements can be removed to 
% speed up operation in smaller jobs. 
% 5. Estimation can be sped up by commenting out code that computes
% the covariance of the estimated fixed effects and t-stats for the 
% estimated fixed effects.
%----------------------------------------------------------------%
% See also: prt_felogit, felogit_lik, ols, logit, mm_felogit
%----------------------------------------------------------------%
% References: A note on maximum likelihood fixed effects 
%   estimation by George Jakubson, available from sdw9@cornell.edu
%----------------------------------------------------------------%

% written by:
% Simon D. Woodcock
% CISER / Economics
% Cornell University
% Ithaca, NY
% sdw9@cornell.edu

%---------------------------------------------------------%
%       ERROR CHECKING AND PRELIMINARY CALCULATIONS       %
%---------------------------------------------------------%

if (nargin < 4); error('Too few arguments to felogit'); end;
if (nargin > 8); error('Too many arguments to felogit'); end;

[nobs junk] = size(y);
[nx nvar] = size(x);
if nobs ~= nx; error('y and x must have the same row dimension'); end;

% create i,t indices
ii = zeros(nobs,1); ii(1,1) = 1; it(1,1) = 1;
for i = 2:nobs;
    it(i,1) = i; % this is original row position index, needed for reassembly in felogitmask
    if ivec(i,1) == ivec(i-1,1)
        ii(i,1) = ii(i-1,1);
    else 
        ii(i,1) = ii(i-1,1) + 1;
    end;
end;
N = max(ii);

if nargin == 4; maxit = 100; tol = 1e-8; 
    res = ols(y,x); beta0 = res.beta; 
    c0 = zeros(N,1); 
end; 
if nargin == 5; maxit = 100; tol = 1e-8; c0 = zeros(N,1); end;

if nargin > 5 % check if beta0 or c0 are empty
    [tmp junk] = size(beta0);
    if tmp == 0; res = ols(y,x); beta0 = res.beta; end;
    [tmp junk] = size(c0);
    if tmp == 0; c0 = zeros(N,1); end;
end;
if nargin == 6; maxit = 100; tol = 1e-8; end;
if nargin == 7; tol = 1e-8; end;    

if ~isnumeric(ivec); error('Argument ivec must be numeric'); end;
if ~isnumeric(tvec); error('Argument tvec must be numeric'); end;

% check for all 1's or all 0's
tmp = find(y==1); chk = length(tmp); [nobs junk] = size(y);
if chk == nobs; error('logit: y-vector contains all ones');
elseif chk == 0; error('logit: y-vector contains no ones'); end;

% create sparse design matrix
d = sparse(1:nobs,ii,1); 

% check for all 1's or all 0's within i -- these observations must be dropped
% or else the algorithm below tries to set their c_i to +/- infinity
T_i = sum(d);                                                       % (1 x N) vector of # observations on each i
tmp = d'*y;                                                         % (N x 1) vector of sum of y_it for each i
[i0,junk] = find(tmp==0);                                           % row indices are {i : y_it=0 , all t}
[i1,junk] = find(tmp==T_i');                                        % row indices are {i : y_it=1 , all t}
n0 = length(i0); n1 = length(i1);
tmp = ~ismember(ii,i0) & ~ismember(ii,i1); 
tmp2 = ~ismember(1:N,i0) & ~ismember(1:N,i1);
save tmp y x;                                                       % save a copy of the original data for testing

% delete observations with all 1's or all 0's
y = y(logical(tmp),:);
x = x(logical(tmp),:);
c0 = c0(logical(tmp2),:);
d = d(logical(tmp),logical(tmp2));
disp(['Observations on ' num2str(n0) ' units were dropped because y_it=0 for all t.']);
disp('The corresponding values of i (from ivec) have been written to the file i0.mat'); 
disp(' ');
savei0 = unique(ivec(logical(ismember(ii,i0)),:)); save i0.mat savei0; clear savei0;
disp(['Observations on ' num2str(n1) ' units were dropped because y_it=1 for all t.']);
disp('The corresponding values of i (from ivec) have been written to the file i1.mat'); 
disp(' ');
savei1 = unique(ivec(logical(ismember(ii,i1)),:)); save i1.mat savei1; clear savei1;
ivec = ivec(logical(tmp),:);
tvec = tvec(logical(tmp),:);

% redefine necessary variables
[nobs junk] = size(y);

% recreate i,t indices
clear ii;
ii = zeros(nobs,1); ii(1,1) = 1;
tt = zeros(nobs,1); tt(1,1) = 1;
for it = 2:nobs;
    if ivec(it,1) == ivec(it-1,1)
        ii(it,1) = ii(it-1,1);
        tt(it,1) = tt(it-1,1) + 1;
    else 
        ii(it,1) = ii(it-1,1) + 1;
        tt(it,1) = 1;
    end;
end;
N = max(ii);
T = max(tt);

% standardize data
xstd = [1 std(x(:,2:nvar))]; 
x = x ./ ( ones(nobs,1)*xstd );  
beta0 = beta0 .* xstd';

% clean up memory before the update
clear ii ivec tt tvec tmp tmp2 junk hunk i0 i1 T_i chk nx res;
pack;


%---------------------------------------------------------%
%  MAXIMUM LIKELIHOOD ESTIMATION OF FIXED EFFECTS LOGIT   %
%---------------------------------------------------------%

b = beta0; 
c = c0;  clear beta0 c0;
iter = 0;

% likelihood and derivatives at starting values
[P,lnL] = felogit_lik(y,x,b,c,d);
[sb sc H11 H12 H22 ] = felogit_deriv(y,x,d,P,nvar,nobs,N);
H22inv = H22 .^ (-1);                                               % H22 is sparse diagonal so invert element by element
H_11 = (H11 - H12*H22inv*H12')\eye(nvar);

% Newton - Raphson update
while (abs(sb'*H_11*sb/length(sb)) > tol) & (iter < maxit)
    iter = iter + 1;
    bold = b;
    cold = c;
    db = - H_11*(sb - H12*H22inv*sc);
    b = bold + db;                                                  % update b
    dc = - H22inv*(sc + H12'*db);
    c = cold + dc;                                                  % update c
    [P,lnL] = felogit_lik(y,x,b,c,d);                               % update P, lnL
    [sb sc H11 H12 H22 ] = felogit_deriv(y,x,d,P,nvar,nobs,N);      % update gradients and Hessian
    H22inv = H22 .^ (-1);                                           % update inv(H22)
    H_11 = (H11 - H12*H22inv*H12')\eye(nvar);                       % update H_11
    disp(['iteration: ' num2str(iter)]);
    disp(['log-likelihood: ' num2str(lnL)]);
end;
clear bold cold db dc i it maxit sb sc;
pack;

%---------------------------------------------------------%
%               GENERATE RESULTS STRUCTURE                %
%---------------------------------------------------------%

results.meth = 'felogit';
results.beta = b ./ xstd';                                          % restore original scale
results.covb = -H_11 ./ (xstd'*xstd);                               % restore original scale
results.stdb = sqrt(diag(results.covb));
results.tstatb = results.beta ./ results.stdb;
results.c = c;
results.yfit = P;
results.lik = lnL;
results.cnvg = tol;
results.iter = iter;
results.nobs = nobs;
results.nvar = nvar;
results.y = y;
results.N = N;
results.T = T;
results.n0 = n0;
results.n1 = n1;

% basic specification testing;
p = sum(y) / nobs;
lnLr = nobs*(p*log(p) + (1-p)*log(1-p));                             % restricted log-likelihood: common intercept ONLY (no slopes)
results.lratio1 = -2*(lnLr - results.lik);                           % LR test based on lnLr
results.rsqr = 1 - (results.lik / lnLr);                             % McFadden pseudo-R^2
load tmp y x; delete tmp.mat;                                        % reload complete data
[nobs junk] = size(x); 
x = [ones(nobs,1) x];
restricted = logit(y,x);                                             % estimate restricted model: no heterogeneity (common intercept)
results.lratio2 = -2*(restricted.lik - results.lik);                 % LR test based on restricted.lik

% The following computes the covariance matrix for the fixed
% effects. It is computationally demanding (and probably not
% useful in most applications) so you may wish to comment it out
   save all.mat; clear; load all.mat H22 H12 H11 N;
   res.covc = -(H22-H12'*(H11\H12))\eye(N);
   res.stdc = sqrt(diag( res.covc ));
   load all.mat results; 
   results.tstatc = results.c ./ res.stdc;
   results.covc = res.covc; results.stdc = res.stdc; 
   clear res H22 H11; pack;
   load all.mat H_11 H22inv xstd; delete all.mat;
   results.covbc = H_11*H12*H22inv ./ (xstd'*ones(1,N));             % restore original scale
% End optional code


%---------------------------------------------------------%
%     SUPPLEMENTARY FUNCTION FOR COMPUTING DERIVATIVES    %
%---------------------------------------------------------%

function [sb,sc,H11,H12,H22] = felogit_deriv(y,x,d,P,nvar,nobs,N);
% PURPOSE: Computes gradient and elements of Hessian for the 
% fixed effects logit model;
% ---------------------------------------------------------

% written by:
% Simon D. Woodcock
% CISER / Economics
% Cornell University
% Ithaca, NY
% sdw9@cornell.edu

% the step variable is introduced to tradeoff memory usage 
% against compute time in large problems
% the size of crit should be reduced on systems with very little RAM
crit=12000000;
if nobs*N < crit
    step = N;
else 
    step = max( round(crit/nobs) , 1);
end;

% gradient vectors
sb = x'*(y-P);
sc = d'*(y-P);

% H11 is top-left nvar x nvar block of Hessian
tmp1 = ( (ones(nobs,1) - P)*ones(1,nvar) ) .* x;
tmp2 = ( P*ones(1,nvar) ) .* x;
H11  = -(tmp1'*tmp2);
clear tmp1;

% H12 is top-right nvar x N block of Hessian
% H22 is bottom-right N x N block of Hessian
% use a for loop to avoid creating (nobs x N) non-sparse matrices
H22 = sparse(N,N);
H12 = zeros(nvar,N);
pack;                                                                  % defragment memory
for i = 1:N/step;                                                      % do this in (nobs x step) blocks to speed things up
    f = (i-1)*step + 1;
    l = i*step;
    tmp3 = ( (ones(nobs,1) - P)*ones(1,step) ) .* d(:,f:l);
    tmp1 = ( P*ones(1,step) ) .* d(:,f:l);
    H12(:,f:l) = - (tmp2'*tmp3);
    H22(f:l,f:l) = - (tmp1'*tmp3);
end;
tmp3 = ( (ones(nobs,1) - P)*ones(1,N-(i*step)) ) .* d(:,(i*step)+1:N); % the remainder
tmp1 = ( P*ones(1,N-(i*step)) ) .* d(:,(i*step)+1:N);
H12(:,(i*step)+1:N) = - (tmp2'*tmp3);
H22((i*step)+1:N,(i*step)+1:N) = - (tmp1'*tmp3);
clear tmp1 tmp2 tmp3 step;

