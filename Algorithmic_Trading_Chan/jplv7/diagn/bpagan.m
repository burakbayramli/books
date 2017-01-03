function result = bpagan(y,x)
% PURPOSE: Breusch-Pagan heteroscedasticity test
%---------------------------------------------------
% USAGE:      result = bpagan(y,x), returns a results structure
%        or:           bpagan(y,x), prints results to command window
% where: y = dependent variable vector (n x 1)
%        x = independent variable matrix (n x k), 
%            (must include a constant)
%---------------------------------------------------
% RETURNS: a structure, or prints to the command window
%        result.meth  = 'bpagan'
%        result.lm    = Breush-Pagan LM-statistic
%        result.dof   = degrees of freedom
%        result.prob  = probability based on chi-squared(k-1)
% --------------------------------------------------
% SEE ALSO: lmtest, waldf
%---------------------------------------------------
% REFERENCES: Breusch and Pagan, Econometrica 1979

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatia-econometrics.com


if nargout == 0
prt = 1;
elseif nargout == 1
prt = 0;
elseif nargin ~= 2
error('Wrong # of arguments to bpagan');
end;

[n, k] = size(x);

ores = ols(y,x);
u2  = ores.resid.*ores.resid;
sig = (sum(u2)/n)*ones(n,1);
f = u2./sig - ones(n,1);
z = x.^2;
lm = .5*f'*z*inv(z'*z)*z'*f;
dof = k-1;

if prt == 0
result.meth = 'bpagan'
result.dof = k-1;
result.lm = lm;
result.prob = 1-chis_prb(lm,k-1);
else
fprintf(1,'Breush-Pagan LM-statistic = %16.8f \n',lm);
fprintf(1,'Chi-squared probability   = %16.4f \n',1-chis_prb(lm,k-1));
fprintf(1,'Degrees of freedom        = %16d   \n',dof);
end;


