function result = logit_large(y,x,maxit,tol)
% PURPOSE: computes Logit Regression
%---------------------------------------------------
% USAGE: results = logit(y,x,maxit,tol)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%    maxit = optional (default=100)
%      tol = optional convergence (default=1e-6)
%---------------------------------------------------
% RETURNS: a structure
%        result.meth   = 'logit'
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
% SEE ALSO: prt(results), probit(), tobit()
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

% modified Hessian by Magnus Myrholm 
% Dept of Economic History, Uppsala, Sweden 
% magnus.myrholm@ekhist.uu.se


if (nargin < 2); error('Wrong # of arguments to logit'); end;
if (nargin > 4); error('Wrong # of arguments to logit'); end;

% check for all 1's or all 0's
tmp = find(y ==1);
chk = length(tmp); 
[nobs junk] = size(y);
if chk == nobs
   error('logit: y-vector contains all ones');
elseif chk == 0
   error('logit: y-vector contains no ones');
end;


% maximum likelihood logit estimation
result.meth = 'logit';

res = ols(y,x); % use ols values as start
[t k] = size(x);
b = res.beta;

if nargin == 2
   tol = 0.000001;
   maxit = 100;
elseif nargin ==3
   tol = 0.000001;
end;

crit = 1.0;
i = ones(t,1);

% changed 
% split up for memory 
tmp1 = [];
for i = 1:k
   tmp = sparse(zeros(t,1)); 
   tmp1 = [tmp1,tmp];
end
tmp2 = tmp1;

%tmp1 = zeros(t,k);
%tmp2 = zeros(t,k);

iter = 1;
while (iter < maxit) & (crit > tol)
   
   tmp = (1+exp(-x*b));
   pdf = exp(-x*b)./(tmp.*tmp);
   cdf = 1./(1+exp(-x*b));
   
   %tmp = (i+exp(-x*b));
   %pdf = exp(-x*b)./(tmp.*tmp);
   %cdf = i./(i+exp(-x*b));
   
   tmp = find(cdf <=0);
   [n1 n2] = size(tmp);
   if n1 ~= 0; cdf(tmp) = 0.00001; end;
   
   tmp = find(cdf >= 1);
   [n1 n2] = size(tmp);
   if n1 ~= 0; cdf(tmp) = 0.99999; end;
   
   % gradient vector for logit 
   %term1 = y.*(pdf./cdf); term2 = (i-y).*(pdf./(i-cdf));
   term1 = y.*(pdf./cdf); term2 = (1-y).*(pdf./(1-cdf));

   for kk=1:k;
      tmp1(:,kk) = term1.*x(:,kk);
      tmp2(:,kk) = term2.*x(:,kk);
   end;
   g = tmp1-tmp2; gs = (sum(g))';
   
   % modifierad Hessian
   delta = sparse(exp(x*b)./(1+exp(x*b))); % see page 883 Green, 1997
   
   % do this instead 
   Yh = sparse(diag(delta.*(1-delta)));
   H = -(sparse(x)'*Yh'*sparse(x));
   
   %H = zeros(k,k);
   %for ii=1:t;
   %xp = x(ii,:)';
   %H = H - delta(ii,1)*(1-delta(ii,1))*(xp*x(ii,:));
   %end;
   
   db = -inv(H)*gs;
   % stepsize determination
   s = 2;
   term1 = 0; term2 = 1;
   while term2 > term1
      s = s/2;
      term1 = lo_like(b+s*db,y,x);
      term2 = lo_like(b+s*db/2,y,x);
   end;
   
   bn = b + s*db;
   crit = abs(max(max(db)));
   b = bn;
   iter = iter + 1;
end; % end of while

% modified
delta = sparse(exp(x*b)./(1+exp(x*b))); % see page 883 Green, 1997

% do this instead 
Yh = sparse(diag(delta.*(1-delta)));
H = -(sparse(x)'*Yh'*sparse(x));

% compute Hessian for inferences
%delta = exp(x*b)./(i+exp(x*b)); % see page 883 Green, 1997
%H = zeros(k,k);
%for i=1:t;
%   xp = x(i,:)';
%   H = H - delta(i,1)*(1-delta(i,1))*(xp*x(i,:));
%end;

% do this instead 
covb = -inv(full(H));
stdb = sqrt(diag(covb));
result.tstat = b./stdb;

% now compute regression results
%covb = -inv(H);
%stdb = sqrt(diag(covb));
%result.tstat = b./stdb;

% fitted probabilities
prfit = 1./(1+exp(-x*b));
result.resid = y - prfit;
result.sige = (result.resid'*result.resid)/t;

% fitted probabilities
%prfit = ones(t,1)./(i+exp(-x*b));
%result.resid = y - prfit;
%result.sige = (result.resid'*result.resid)/t;

% find ones
tmp = find(y ==1);
P = length(tmp); 
cnt0 = t-P;
cnt1 = P;
P = P/t; % proportion of 1's
like0 = t*(P*log(P) + (1-P)*log(1-P)); % restricted likelihood
like1 = lo_like(b,y,x); % unrestricted Likelihood

result.r2mf = 1-(abs(like1)/abs(like0)); % McFadden pseudo-R2 
term0 = (2/t)*like0;
term1 = 1/(abs(like1)/abs(like0))^term0;
result.rsqr = 1-term1;  % Estrella R2

result.beta = b;
result.yhat = prfit;
result.lratio = 2*(like1-like0); % LR-ratio test against intercept model
result.lik   = like1;% unrestricted Likelihood
result.nobs  = t;    % nobs
result.nvar  = k;    % nvars
result.zip   = cnt0; % number of 0's
result.one   = cnt1; % number of 1's
result.iter  = iter; % number of iterations
result.convg = full(crit); % convergence criterion max(max(-inv(H)*g))
result.y = y;        % y data vector
