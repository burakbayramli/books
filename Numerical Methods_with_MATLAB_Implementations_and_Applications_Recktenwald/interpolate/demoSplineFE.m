function demoSplineFE(nk)
% demoSplineFE  Spline approx to y = x*exp(-x) with fixed slope end conditions
%
% Synopsis:    demoSplineFE
%
% Input:       nk = (optional) number of knots used to define the interpolant
%                   Default:  nk = 4 
%
% Output:      Plot of cubic spline approximations to y = x*exp(-x)
%              Splines with zero slope and exact slope at end points are
%              compared to the exact function.

if nargin<1,  nk = 4;  end

x = linspace(0,5,nk);           %  vector of knots
y = x.*exp(-x);                 %  f(x) at knots
xi = linspace(min(x),max(x))';  %  Evaluate interpolant at xi
ye = xi.*exp(-xi);              %  Exact f(x) for comparison

n = length(x);
yp1 = (1-x(1))*exp(-x(1));      %  Exact slope at x(1)
ypn = (1-x(n))*exp(-x(n));      %     and at x(n)
yi = splintFE(x,y,xi,yp1,ypn);  %  Spline with exact slope end conditions
errExs = norm(yi-ye);
yi0 = splintFE(x,y,xi,0,0);     %  Spline with zero slope end conditions
errZs = norm(yi0-ye);
fprintf('\nFor %d knots:\n   Error with exact slope = %10.2e\n',nk,errExs);
fprintf('   Error with zero slope  = %10.2e\n',errZs);

plot(x,y,'bo',xi,ye,'b-',xi,yi,'r--',xi,yi0,'b-.');
legend('Knots','x*exp(-x)','exact slope','zero slope');
text(4,0.2,sprintf('%d knots',nk));
axis([0 x(n) 0 0.5]);
