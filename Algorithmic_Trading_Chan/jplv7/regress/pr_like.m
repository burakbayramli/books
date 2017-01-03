function like = pr_like(b,y,x)
% PURPOSE: evaluate probit log-likelihood
%-----------------------------------------------------
% USAGE:    like = pr_like(b,y,x,flag) 
% where:     b = parameter vector (k x 1)
%            y = dependent variable vector (n x 1)
%            x = explanatory variables matrix (n x m)
%-----------------------------------------------------
% NOTE: this function returns a scalar
%-----------------------------------------------------
% SEE also: hessian, gradnt, gradt
%-----------------------------------------------------
% REFERENCES: Green, 1997 page 883
%-----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% error check
if nargin ~= 3
 error('wrong # of arguments to pr_like');
end;
[m junk] = size(b);
if junk ~= 1
 error('pr_like: requires a column vector');
end;

i = ones(length(y),1);

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


out = y.*log(cdf)+(i-y).*log(i-cdf);
like = sum(out);


