function rresid = recresid(y,x)
% PURPOSE: compute recursive residuals
%----------------------------------------------
% USAGE: rresid = recresid(y,x)
% where: y = dependent variable vector (n x 1)
%        x = explanatory variables matrix (n x k)
%----------------------------------------------
% RETURNS: rresid = recursive residuals
%                   (first k-obs equal zero)
% ---------------------------------------------
% REFERENCES: A.C. Harvey, (1981) The Econometric Analysis
%              of Time-Series, pp. 54-56.
%----------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatia-econometrics.com

if nargin ~= 2
error('Wrong # of arguments to recresid');
end;

[n k] = size(x);
rresid = zeros(n,1);
b = zeros(n,k);

% initial estimates based on first k observations
xxt1 = inv(x(1:k,1:k)'*x(1:k,1:k));
bt1 = xxt1*x(1:k,1:k)'*y(1:k,1);
b(k,:) = bt1';
j = k+1;

while j <= n
xt = x(j,1:k); yt = y(j,1);
ft = 1 + xt*xxt1*xt';
xxt = xxt1 - xxt1*xt'*xt*xxt1/ft;
vt = yt - xt*bt1;
bt = bt1 + xxt1*xt'*vt/ft;
% save results and prepare for next loop
rresid(j,1) = vt/sqrt(ft);
b(j,:) = bt';
xxt1 = xxt;
bt1 = bt;
j = j+1;
end;