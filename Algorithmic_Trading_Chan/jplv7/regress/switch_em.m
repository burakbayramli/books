function results = switch_em(y,x1,x2,x3,b1,b2,b3,crit,maxit)
% PURPOSE: Switching Regime regression (EM-estimation)
%          y1 = x1*b1 + e1
%          y2 = x2*b2 + e2
%          y3 = x3*b3 + e3; e3 =N(0,1) 
%          y = y1 if y3 <= 0, y = y2 if y3 > 0 
%---------------------------------------------------
% USAGE: results = switch_em(y,x1,x2,x3,crit)
% where: y = dependent variable vector (nobs x 1)
%        x1 = independent variables matrix (nobs x k1)
%        x2 = independent variables matrix (nobs x k2)
%        x3 = independent variables matrix (nobs x k3)
%        b1 = (optional) initial values for b1
%        b2 = (optional) initial values for b2
%        b3 = (optional) initial values for b3
%      crit = (optional) convergence criteria (default = 0.001)  
%     maxit = maximum # of iterations (default = 1000)     
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'switch_em'
%        results.beta1 = bhat1
%        results.beta2 = bhat2
%        results.beta3 = bhat3
%        results.t1    = t-stats for bhat1
%        results.t2    = t-stats for bhat2
%        results.t3    = t-stats for bhat3
%        results.y1    = predicted values regime 1
%        results.y2    = predicted values regime 2
%        results.yhat  = yhat based on probs > 0.5
%        results.r1    = residuals regime 1
%        results.r2    = residuals regime 2
%        results.resid = residuals based on probs > 0.5
%        results.sig1  = e1'*e1/(n1-k1)
%        results.sig2  = e2'*e2/(n2-k2)
%        results.rsqr1 = rsquared, regime 1
%        results.rsqr2 = rsquared, regime 2
%        results.nobs  = nobs
%        results.k1    = k1, variables in x1
%        results.k2    = k2, variables in x2
%        results.k3    = k3, variables in x3
%        results.nvar  = k1+k2+k3
%        results.y     = y data vector
%        results.prob1 = probability of regime 1
%        results.prob2 = probability of regime 2
%        results.iter  = # of iterations in EM-algorithm
%        results.crit  = convergence criterion
%        results.like  = likelihood function value
% --------------------------------------------------
% SEE ALSO: prt,plt switchm_em 
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu


if nargin == 4
    bflag = 0;
    crit = 0.001;
    maxit = 1000;
elseif nargin == 7
    bflag = 1;
    crit = 0.001;
    maxit = 1000;
elseif nargin == 8
    bflag = 1;
    maxit = 1000;
elseif nargin == 9
    bflag = 1;
else
error('Wrong # of arguments to switch_em');
end;

[n1 k1] = size(x1);
[n2 k2] = size(x2);
[n3 k3] = size(x3);

if n1 ~= n2
error('switch_em: x1, x2 have different nobs');
elseif n2 ~= n3
error('switch_em: x2, x3 have different nobs');
end;

nobs = n1;

% start EM-loop
converge = 1.0;
iter = 0;

if bflag == 0 % user supplied NO initial values

% get  starting values using postiive and negative y's
ypos = find(y > 0);
yneg = find(y <= 0);

res = ols(y(ypos,1),x1(ypos,:));
sig1 = res.sige;
b1 = res.beta;
res = ols(y(yneg,1),x2(yneg,:));
sig2 = res.sige;
b2 = res.beta;

% b3 starting values using ad-hockery
res = ols(y,x3);
b3 = res.beta;

else % user supplied b1,b2,b3 we need to find sig1,sig2
    sig1 = (y - x1*b1)'*(y - x1*b1)/nobs;
    sig2 = (y - x2*b2)'*(y - x2*b2)/nobs; 
end;

while converge > crit

f1=norm_pdf(((y-x1*b1)/sig1)/sig1);
f2=norm_pdf(((y-x2*b2)/sig2)/sig2);
lamda=norm_cdf(-x3*b3);
h=(lamda.*f1)+((1-lamda).*f2);
chk = find(h == 0);
if length(chk > 0)
h(chk) = 0.01;
end;
w1=lamda.*f1./h;
w2=ones(nobs,1)-w1;
epsilon=x3*b3+(f2-f1).*norm_pdf(-x3*b3./h);

t1=sum(w1);
t2=sum(w2);
if t1 <= 1
    error('switch_em: regime 1 weights are all near zero');
end;
if t2 <= 1
    error('switch_em: regime 2 weights are all near zero');
end;

w1 = sqrt(w1);
w2 = sqrt(w2);


xx1=matmul(x1,w1);
xx2=matmul(x2,w2);
y1=y.*w1;
y2=y.*w2;
b01 = b1;
b02 = b2;
b03 = b3;

b1=inv(xx1'*xx1)*xx1'*y1;
b2=inv(xx2'*xx2)*xx2'*y2;
b3=inv(x3'*x3)*x3'*epsilon;

sig01 = sig1;
sig02 = sig2;

sig1=y1-xx1*b1;
sig2=y2-xx2*b2;
sig1=sqrt(sig1'*sig1/t1);
sig2=sqrt(sig2'*sig2/t2);

% check for convergence
c = max(abs(b1-b01));
c = max(c,max(abs(b2-b02)));
c = max(c,max(abs(b3-b03)));
c = max(c,max(sig1-sig01));
converge = max(c,max(sig2-sig02));

iter = iter + 1;
if iter > maxit
warn('switch_em: maximum # of iterations exceeded');
break;
end;

end;

% compute t-statistics
tmp = sig1*(inv(xx1'*xx1));
results.t1 = b1./sqrt(diag(tmp));
tmp = sig2*(inv(xx2'*xx2));
results.t2 = b2./sqrt(diag(tmp));
tmp = inv(x3'*x3); % sig3 = 1
results.t3 = b3./sqrt(diag(tmp));

% return results
results.meth  = 'switch_em';
results.iter  = iter;
results.crit  = converge;
results.beta1 = b1;
results.beta2 = b2;
results.beta3 = b3;
results.nobs  = nobs;
results.k1    = k1;
results.k2    = k2;
results.k3    = k3;
results.nvar = k1+k2+k3;
results.y     = y;
results.prob1 = w1.^2;
results.prob2 = w2.^2;
results.y1    = xx1*b1;
results.y2    = xx2*b2;
results.r1    = y-results.y1;
results.r2    = y-results.y2;
results.sig1  = sig1;
results.sig2  = sig2;

% compute R-squared based on two regimes
sigu1 = 0; sigu2 = 0;
y1sum = 0; y2sum = 0;
y1s = 0; y2s = 0;
prob1 = results.prob1;
nobs1 = 0; nobs2 = 0;
for i=1:nobs
    if prob1(i,1) > 0.5
        sigu1 = sigu1 + results.r1(i,1)*results.r1(i,1);
        y1sum = y1sum + y(i,1);
        y1s = y1s + y1sum*y1sum;
        nobs1 = nobs1+1;
    else
        sigu2 = sigu2 + results.r2(i,1)*results.r2(i,1);
        y2sum = y2sum + y(i,1);
        y2s = y2s + y2sum*y2sum;
        nobs2 = nobs2+1;
    end;
end;

% Overall y-hat and  R-squared 
yhat  = zeros(nobs,1);
resid = zeros(nobs,1);
for j=1:2
 for i=1:nobs
  if prob1 >= 0.5 
  yhat(i,1) = xx1(i,:)*b1;
  resid(i,1) = y(i,1) - yhat(i,1);
  else
  yhat(i,1) = xx2(i,:)*b2;
  resid(i,1) = y(i,1) - yhat(i,1);    
  end;
 end;
end;

results.yhat = yhat;
results.resid = resid;
results.rsqr1 = 1 - sigu1/y1s; % r-squared
results.rsqr2 = 1 - sigu2/y2s;

% log likelihood
like = log((1-lamda).*f1 + lamda.*f2);
results.like = sum(like);
