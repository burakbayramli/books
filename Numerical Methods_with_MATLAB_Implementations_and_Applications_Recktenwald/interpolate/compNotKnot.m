function compNotKnot(n)
% compNotKnot  Compare implementations of splines with not-a-knot end conditions
%              NMM toolbox routine splint, is compared with built in spline
%              function in the interpolation of y=x*exp(-x) on [0,5]
%
% Synopsis:  compSplinePlot
%            compSplinePlot(n)
%
% Input:     n = (optional) number of knots in the range 0 <= x <= 5
%                Default:  n=6
%
% Output:    Plot of spline approximations to y = x*exp(-x) with not-a-knot,
%            natural, zero slope, and exact slope end conditions.  Normalized
%            errors for each interpolant are also computed and printed

if nargin<1, n=6;  end

x = linspace(0,5,n)';            %  vector of knots
y = x.*exp(-x);                  %  f(x) at knots
xi = linspace(min(x),max(x))';   %  Evaluate spline at these xi
ye = xi.*exp(-xi);               %  Exact f(x) at the xi

tic
yi = splint(x,y,xi);             %  NMM routine
tNMM = toc;
errNMM = norm(yi-ye);

tic
yim = spline(x,y,xi);            %  built in not-a-knot spline
tSpline = toc;
errSpline = norm(yim-ye);

fprintf('                                time       error\n');
fprintf('  NMM splint function         %7.4f   %10.2e\n',tNMM,errNMM);
fprintf('  Built in spline function    %7.4f   %10.2e\n',tSpline,errSpline);

subplot(1,2,1);
plot(x,y,'bo',xi,yi,'r--',xi,yim,'g-');
axis([0 6 0 0.5]);
legend('knots','NMM splint','built in spline');
title('Not-a-knot end conditions');

subplot(1,2,2);
plot(xi,yi-yim);
