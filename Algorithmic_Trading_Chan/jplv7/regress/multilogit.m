function results = multilogit(y,x,beta0,maxit,tol);
% PURPOSE: implements multinomial logistic regression
% Pr(y_i=j) = exp(x_i'beta_j)/sum_l[exp(x_i'beta_l)]
%   where:
%   i    =   1,2,...,nobs
%   j,l  = 0,1,2,...,ncat
%-------------------------------------------------------------------------%
% USAGE: results = multilogit(y,x,beta)
% where: y = response variable vector (nobs x 1)
%            the response variable should be coded sequentially from 0 to
%            ncat, i.e., y in {0,1,2,...,ncat}
%        x = matrix of covariates (nobs x nvar)
%            NOTE: to include a constant term in each beta_j,
%            include a column of ones in x
%    beta0 = optional starting values for beta (nvar x ncat+1) (default=0)
%    maxit = optional maximum number of iterations (default=100)
%      tol = optional convergence tolerance (default=1e-6)
%-------------------------------------------------------------------------%
% RETURNS: a structure
%        results.meth = 'multilogit'
%    results.beta_mat = (nvar x ncat) matrix of beta coefficients:
%                       [beta_1 beta_2 ... beta_ncat] under the 
%                       normalization beta_0 = 0
%    results.beta_vec = (nvar*ncat x 1) vector of beta coefficients:
%                       [beta_1 ; beta_2 ; ... ; beta_ncat] under
%                       normalization beta_0 = 0
%        results.covb = (nvar*ncat x nvar*ncat) covariance matrix
%                       of results.beta_vec
%   results.tstat_mat = matrix of t-statistics conformable to 
%                       results.beta_mat
% results.tstat_vec   = vector of t-statistics conformable to
%                       results.beta_vec
%        results.yfit = (nobs x ncat+1) matrix of fitted 
%                       probabilities: [P_0 P_1 ... P_ncat]
%                       where P_j = [P_1j ; P_2j ; ... ; P_nobsj]
%         results.lik = unrestricted log likelihood
%        results.cnvg = convergence criterion
%        results.iter = number of iterations
%        results.nobs = number of observations
%        results.nvar = number of variables
%        results.ncat = number of categories of dependent variable
%                       (including the reference category j = 0)
%       results.count = vector of counts of each value taken by y, i.e., 
%                       count = [#y=0 #y=1 ... #y=ncat]
%           results.y = y vector
%      results.lratio = LR test statistic against intercept-only model (all 
%                       betas=0), distributed chi-squared with (nvar-1)*ncat
%                       degrees of freedom
%        results.rsqr = McFadden pseudo-R^2
%      
%-------------------------------------------------------------------------%
% A NOTE: Since users might prefer results (coefficients and tstats) in 
%   either a vector or matrix format, and since there is no single natural
%   representation for these in the multinomial logit model, the results
%   structure returns both.  Note that the input arguments require that 
%   (optional) starting values in matrix (nvar x ncat) format.
%
%-------------------------------------------------------------------------%
% SEE ALSO: prt_multilogit, multilogit_lik
%-------------------------------------------------------------------------%
% References: Greene (1997), p.914

% written by:
% Simon D. Woodcock
% CISER / Economics
% Cornell University
% Ithaca, NY 
% sdw9@cornell.edu

%---------------------------------------------------------%
%       ERROR CHECKING AND PRELIMINARY CALCULATIONS       %
%---------------------------------------------------------%

if nargin < 2, error('multilogit: wrong # of input arguments'); end;
y = round(y(:)); [nobs cy]=size(y); [rx nvar]=size(x);

if (rx~=nobs), error('multilogit: row dimensions of x and y must agree'); end;

% initial calculations
xstd = [1 std(x(:,2:nvar))];
x = x ./ ( ones(nobs,1)*xstd );                           % standardize x
ymin = min(y);
ymax = max(y);
ncat = ymax - ymin;
d0 = ( y*ones(1,ncat+1) ) == ( ones(nobs,1)*(ymin:ymax) );  % put y in dummy format
d = d0(:,2:ncat+1);                                         % normalize beta_0 = 0

% starting values

if nargin < 3
    beta0 = zeros(nvar,ncat+1);
else 
    [a b] = size(beta0);
    if a == 0
        beta0 = zeros(nvar,ncat+1);
    else for j = 1:ncat;
            beta0(:,j) = beta0(:,j) .* xstd';
         end;
    end;
end;

beta = beta0(:,2:ncat+1);

% default max iterations and tolerance
if nargin < 4 , maxit = 100; tol = 1e-6; end;
if nargin < 5 , tol = 1e-6; end;

if nargin > 6 , error('multilogit: wrong # of arguments'); end;

% check nvar and ncat are consistently defined;
[rbeta cbeta] = size(beta);
if nvar ~= rbeta
    error('multilogit: rows of beta and columns of x do not agree')
end;
if ncat ~= cbeta
    error(['multilogit: number of columns in beta and categories in y do not agree. ' ...
        'check that y is numbered continuously, i.e., y takes values in {0,1,2,3,4,5}' ...
        ' is ok, y takes values in {0,1,2,3,4,99} is not.'])
end;

%----------------------------------------------------%
% MAXIMUM LIKELIHOOD ESTIMATION OF MULTINOMIAL LOGIT %
%----------------------------------------------------%

% likelihood and derivatives at starting values
[P,lnL] = multilogit_lik(y,x,beta,d);
[g H] = multilogit_deriv(x,d,P,nvar,ncat,nobs);

iter=0;

for j = 1:ncat % vectorize beta and gradient for newton-raphson update
    f = (j-1)*nvar + 1;
    l = j*nvar;
    vb(f:l,1) = beta(:,j);
    vg(f:l,1) = g(:,j);
end;

% newton-raphson update
while (abs(vg'*(H\vg)/length(vg)) > tol) & (iter < maxit)
    iter = iter + 1;
    betaold = beta;
    vbold = vb;
    vb = vbold - H\vg;
    for j = 1:ncat                                   % de-vectorize updated beta for pass to multilogit_lik
        f = (j-1)*nvar + 1;
        l = j*nvar;
        beta(:,j) = vb(f:l,1);
    end;
    [P,lnL] = multilogit_lik(y,x,beta,d);            % update P, lnL
    [g H] = multilogit_deriv(x,d,P,nvar,ncat,nobs);  % update g,H
    for j = 1:ncat;                                  % vectorize updated g for next N-R update
        f = (j-1)*nvar + 1;
        l = j*nvar;
        vg(f:l,1) = g(:,j);
    end;
    disp(['iteration: ' num2str(iter)]);
    disp(['log-likelihood: ' num2str(lnL)]);
end;

%---------------------------------------------------------%
%               GENERATE RESULTS STRUCTURE                %
%---------------------------------------------------------%

results.meth = 'multilogit';
for j = 1:ncat
    results.beta_mat(:,j) = beta(:,j) ./ xstd';        % restore original scale
end;
for j = 1:ncat
    f = (j-1)*nvar + 1;
    l = j*nvar;
    results.beta_vec(f:l,1) = results.beta_mat(:,j);
end;
results.covb = -inv(H)./kron(ones(ncat),(xstd'*xstd)); % restore original scale
stdb = sqrt(diag(results.covb));
results.tstat_vec = results.beta_vec./stdb;
for j = 1:ncat                                  
    f = (j-1)*nvar + 1;
    l = j*nvar;
    results.tstat_mat(:,j) = results.tstat_vec(f:l,1);
end;
P_0 = ones(nobs,1) - sum(P')';
results.yfit = [P_0 P];
results.lik = lnL;
results.cnvg = tol;
results.iter = iter;
results.nobs = nobs;
results.nvar = nvar;
results.ncat = ncat;
results.count = [nobs-sum(sum(d)') sum(d)];
results.y = y;

% basic specification testing;
p = results.count / nobs;
lnLr = nobs*sum((p.*log(p))'); % restricted log-likelihood: intercepts only
results.lratio = -2*(lnLr - results.lik);
results.rsqr = 1 - (results.lik / lnLr); % McFadden pseudo-R^2




%---------------------------------------------------------%
%     SUPPLEMENTARY FUNCTION FOR COMPUTING DERIVATIVES    %
%---------------------------------------------------------%

function [g,H] = multilogit_deriv(x,d,P,nvar,ncat,nobs);
% PURPOSE: Computes gradient and Hessian of multinomial logit 
% model
% ---------------------------------------------------------
% References: Greene (1997), p.914

% written by:
% Simon D. Woodcock
% CISER / Economics
% 201 Caldwell Hall
% Cornell University
% Ithaca, NY 14850
% sdw9@cornell.edu

% compute gradient matrix (nvar x ncat)
tmp = d - P;
g = x'*tmp;

% compute Hessian, which has (ncat)^2 blocks of size (nvar x nvar)
% this algorithm builds each block individually, m&n are block indices
H = zeros(nvar*ncat);
for m = 1:ncat; 
    for n = 1:ncat;
        fr = (m-1)*nvar + 1;
        lr = m*nvar;
        fc = (n-1)*nvar + 1;
        lc = n*nvar;       
        index = (n==m);
        index = repmat(index,nobs,1);
        H(fr:lr,fc:lc) = -( ( x.*( P(:,m)*ones(1,nvar) ) )' * ( x.*( (index-P(:,n))*ones(1,nvar) ) ) ) ;
    end;
end;
