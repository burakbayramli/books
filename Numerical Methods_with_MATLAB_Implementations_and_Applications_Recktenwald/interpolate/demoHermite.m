function demoHermite(n)
% demoHermite  Cubic Hermite interpolation of y = x*exp(-x) for 0 <= x <= 8
%
% Synopsis:    demoHermite
%              demoHermite(n)
%
% Input:       n = (optional) number of knots used to define the interpolant
%                  Default:  n = 4 
%
% Output:      Plot of cubic Hermite approximation to y = x * exp(-x)

if nargin<1,  n = 4;  end

x = linspace(0,8,n);           %  vector of knots
y = x.*exp(-x);                %  f(x)
yp = (1 - x).*exp(-x);         %  f'(x)
xi = linspace(min(x),max(x));  %  Evaluate interpolant at xi
ye = xi.*exp(-xi);             %  Exact f(x) for comparison

yi = hermint(x,y,yp,xi);
err = norm(yi-ye(:));
fprintf('error = %12.2e  with %d knots\n',err,n);

plot(x,y,'bo',xi,ye,'b-',xi,yi,'r--');
legend('Given','x*exp(-x)','Hermite');
text(4,0.2,sprintf('%d knots',n));
axis([0 8 0 0.5]);
