function y = fdis_prb( x, m, n)
% PURPOSE: computes f-distribution probabilities
%---------------------------------------------------
% USAGE: y = fdis_prb(x,m,n)
% INPUTS:  x = calculated f-statistic
%          m = numerator dof
%          n = denominator dof
%---------------------------------------------------
% RETURNS:
%        y = a marginal probability level
% --------------------------------------------------
% SEE ALSO: fdis_d, tdis_prb(), chis_prb()
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if nargin ~= 3
error('Wrong # of arguments to fdis_prb');
end;

[np junk] = size(x);
y = zeros(np,1);

for i=1:np;
 if (x(i) < 0 | m <= 0 | n <= 0)
  error('negative or zero dof in fdis_prb');
 else
  %y(i,1) = betai(.5*n,.5*m,n/(n+m*x(i,1)));
tst = n/(n+m*x(i,1));
if tst == 0
tst = tst + 0.0001;
elseif tst == 1
tst = tst - 0.0001;
end;
y(i,1) = betainc(tst,.5*n,.5*m);
    end;
end;    