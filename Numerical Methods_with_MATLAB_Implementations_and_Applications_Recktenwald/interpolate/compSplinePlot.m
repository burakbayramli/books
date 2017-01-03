function compSplinePlot(n)
% compSplinePlot  Compare end conditions for cubic-spline interpolants
%                 Approximations to y = x*exp(-x) are constructed and plotted
%
% Synopsis:  compSplinePlot
%            compSplinePlot(n)
%
% Input:     n = (optional) number of knots in the range 0 <= x <= 5
%                Default:  n=6
%
% Output:    Plot of spline approximations to y = x*exp(-x) with not-a-knot,
%            natural, zero-slope, and exact-slope end conditions.  Normalized
%            errors for each interpolant are also computed and printed

if nargin<1, n=6;  end

x = linspace(0,5,n)';               %  Generate discrete data set
y = x.*exp(-x);
xi = linspace(min(x),max(x))';      %  Evaluate spline at these xi
ye = xi.*exp(-xi);                  %  Exact f(x) at the xi

yi = splint(x,y,xi,'natural');      %  Spline with natural end conditions
errNat = norm(yi-ye)
subplot(2,2,1);  plot(x,y,'bo',xi,ye,'b-',xi,yi,'r--');  axis([0 6 0 0.5]);
legend('knots','spline','x*exp(-x)');  title('Natural end conditions');

yi = splint(x,y,xi,0,0);            %  Spline with zero-slope end conditions
errz = norm(yi-ye)
subplot(2,2,2);  plot(x,y,'bo',xi,ye,'b-',xi,yi,'r--');  axis([0 6 0 0.5]);
legend('knots','spline','x*exp(-x)');  title('Zero-slope end conditions');

yi = splint(x,y,xi);                %  Spline with not-a-knot end conditions
errNot = norm(yi-ye)
subplot(2,2,3);  plot(x,y,'bo',xi,ye,'b-',xi,yi,'r--');  axis([0 6 0 0.5]);
legend('knots','spline','x*exp(-x)');  title('Not-a-knot end conditions');

yp1 = (1-x(1))*exp(-x(1));          %  Exact slope at x(1)
ypn = (1-x(n))*exp(-x(n));          %  and at x(n)
yi = splint(x,y,xi,yp1,ypn);        %  Spline with exact-slope end conditions
errExs = norm(yi-ye)
subplot(2,2,4);  plot(x,y,'bo',xi,ye,'b-',xi,yi,'r--');  axis([0 6 0 0.5]);
legend('knots','spline','x*exp(-x)');  title('Exact-slope end conditions');
