function result = probit_large(y,x,maxit,tol)
% PURPOSE: computes Probit Regression
%---------------------------------------------------
% USAGE: results = probit(y,x,maxit,tol)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%    maxit = optional (default=100)
%      tol = optional convergence (default=1e-6)
%---------------------------------------------------
% RETURNS: a structure
%        result.meth   = 'probit'
%        result.beta   = bhat
%        result.tstat  = t-stats
%        result.yhat   = yhat
%        result.resid  = residuals
%        result.sige   = e'*e/n
%        result.r2mf   = McFadden pseudo-R^2
%        result.rsqr   = Estrella R^2
%        result.lratio = LR-ratio test against intercept model
%        result.lik    = unrestricted Likelihood
%        result.cnvg   = convergence criterion, max(max(-inv(H)*g))
%        result.iter   = # of iterations
%        result.nobs   = nobs
%        result.nvar   = nvars
%        result.zip    = # of 0's
%        result.one    = # of 1's
%        result.y      = y data vector
% --------------------------------------------------
% SEE ALSO: prt_reg(results), logit(), tobit()
%---------------------------------------------------
% References: Arturo Estrella (1998) 'A new measure of fit
% for equations with dichotmous dependent variable', JBES,
% Vol. 16, #2, April, 1998.

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if (nargin < 2); error('Wrong # of arguments to probit'); end;
if (nargin > 4); error('Wrong # of arguments to probit'); end;

% check for all 1's or all 0's
tmp = find(y ==1);
chk = length(tmp); 
[nobs junk] = size(y);
if chk == nobs
   error('probit: y-vector contains all ones');
elseif chk == 0
   error('probit: y-vector contains no ones');
end;

% maximum likelihood probit estimation
result.meth = 'probit';

options = foptions;

%res = ols(y,x); % use ols values as start
[t k] = size(x);
%b = res.beta;
xxi = x'*x;
xxi = inv(xxi); 
xy = x'*y;
b = xxi*xy;


if nargin == 2
   tol = 0.000001;
   maxit = 100;
elseif nargin ==3
   tol = 0.000001;
end;

crit = 1.0;
%i = ones(t,1);

% changed
% split up to avoid high peak memory
%tmp1 = [];
%for i = 1:k
%   tmp = sparse(zeros(t,1)); 
%   tmp1 = [tmp1,tmp];
%end
%tmp2 = tmp1;

%tmp1 = zeros(t,k);
%tmp2 = zeros(t,k);

iter = 1;

while (iter <= maxit) & (crit > tol)
   
   pdf = norm_pdf(x*b);
   cdf = norm_cdf(x*b);
   
   tmp = find(cdf <=0);
   [n1 n2] = size(tmp);
   if n1 ~= 0
      cdf(tmp,1) = 0.00001*ones(length(tmp),1);
   end;
   
   tmp = find(cdf >= 1);
   [n1 n2] = size(tmp);
   if n1 ~= 0
      cdf(tmp,1) = 0.99999*ones(length(tmp),1);
   end;
   
 
   %term1 = y.*(pdf./cdf);
   %term2 = (1-y).*(pdf./(1-cdf));
   
   %for kk=1:k;
   %   tmp1(:,kk) = term1.*x(:,kk);
   %   tmp2(:,kk) = term2.*x(:,kk);
   %end;
   %g = tmp1-tmp2;
   %gs = (sum(g))';
   
   % compute see page 883 Green, 1997
   q = 2*y - 1;  
   xxb = x*b;
   pdf = norm_pdf(q.*xxb);
   cdf = norm_cdf(q.*xxb);
   lambda = sparse((q.*pdf)./cdf); 
   
   % changed gradient vector
   %for i = 1:k
   %   gs(i,1)=sum(lambda.*x(:,i));
   %end
   
   L = sparse(diag(lambda));
   gs = L*x;
   gs = sum(gs)';

   % changed Hessian
   Yh = sparse(diag(lambda.*(lambda + x*b)));
   H = full(-(sparse(x)'*Yh*sparse(x)));
   
   %H = zeros(k,k);
   %for ii=1:t;
   %   xb = x(ii,:)*b;
   %   xp = x(ii,:)';
   %   H = H - lambda(ii,1)*(lambda(ii,1) +xb)*(xp*x(ii,:));
   %end;
   
   db = -inv(H)*gs;
   % stepsize determination
   s = 2;
   term1 = 0; term2 = 1;
   while term2 > term1
      s = s/2;
      term1 = pr_like(b+s*db,y,x);
      term2 = pr_like(b+s*db/2,y,x);
   end;
   
   bn = b + s*db;
   crit = abs(max(max(db)));
   b = bn;
   iter = iter + 1;
   
end; % end of while

if iter >=maxit
   fprintf(1,'probit: no convergence in %d iterations \n',iter);
end;

% compute Hessian for inferences
q = 2*y - 1;  % see page 883 Green, 1997
xxb = x*b;
pdf = norm_pdf(q.*xxb);
cdf = norm_cdf(q.*xxb);
lambda = sparse((q.*pdf)./cdf); 

% changed Hessian
Yh = sparse(diag(lambda.*(lambda + x*b)));
H = full(-(sparse(x)'*Yh'*sparse(x)));

%H = zeros(k,k);
%for i=1:t;
%   xb = x(i,:)*b;
%   xp = x(i,:)';
%   H = H - lambda(i,1)*(lambda(i,1) + xb)*(xp*x(i,:));
%end;

% now compute regression results
covb = -inv(H);
result.covb = covb;
stdb = sqrt(diag(covb));
result.tstat = b./stdb;

% fitted probabilities
result.yhat = norm_cdf(x*b);
result.resid = y - result.yhat;

result.sige = (result.resid'*result.resid)/t;

% find ones
tmp = find(y ==1);
P = length(tmp); 
cnt0 = t-P;
cnt1 = P;
P = P/t; % proportion of 1's
like0 = t*(P*log(P) + (1-P)*log(1-P)); % restricted likelihood
like1 = pr_like(b,y,x);              % unrestricted Likelihood

result.r2mf = 1-(abs(like1)/abs(like0)); % McFadden pseudo-R2 


term0 = (2/t)*like0;
term1 = 1/(abs(like1)/abs(like0))^term0;
result.rsqr = 1-term1;  % Estrella R2

result.beta = b;
result.lratio = 2*(like1-like0); % LR-ratio test against intercept model
result.lik = like1; % unrestricted Likelihood
result.nobs = t;    % nobs
result.nvar = k;    % nvars
result.zip = cnt0;  % number of 0's
result.one = cnt1;  % number of 1's
result.iter = iter; % number of iterations
result.convg = crit;% convergence criterion max(max(-inv(H)*g))
result.y = y;       % y data vector


