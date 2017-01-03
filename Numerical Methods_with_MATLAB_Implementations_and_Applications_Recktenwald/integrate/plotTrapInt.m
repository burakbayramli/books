function I = plotTrapInt(fun,a,b,npanel)
% plotTrapInt  Graphical display of composite trapezoid rule integration
%
% Synopsis:   I = plotTrapInt(fun,a,b)
%             I = plotTrapInt(fun,a,b,npanel)
%
% Input:     fun  = (string) name of m-file that evaluates f(x)
%            a,b  = lower and upper limits of the integral
%            npanel = number of panels to use in the integration
%                     Default: npanel = 4
%
% Output:     I = integral of 'fun(x)' from a to b
%             In addition a plot of the trapezoid areas that add up
%             to the integral is shown

if nargin<4,  npanel=4;  end

% --- evaluate integral
n = npanel + 1;      %  total number of nodes
h = (b-a)/(n-1);     %  stepsize
x = a:h:b;           %  divide the interval
y = feval(fun,x);    %  evaluate integrand;  y is a row vector

I = h * ( 0.5*y(1) + sum(y(2:n-1)) + 0.5*y(n) );

% --- Plot filled boxes, then overlay f(x)
colr = [0.8  0.8  1; ...    %  colr contains two RGB color sets
        0.92 0.92 1];       %  1st row is darker blue, 2nd row is lighter
hold on
for k=1:length(x)-1;
  ic = 1 + rem(k,2);        %  ic=1 if k is even, ic=2 if k is odd
  fill([x(k) x(k+1) x(k+1) x(k) x(k)],...            %  Mark with filled box
       [ 0     0    y(k+1) y(k)  0   ],colr(ic,:));
end

xex = linspace(a,b);        %  x values for smooth curve of exact f(x)
yex = feval(fun,xex);       %  y values  "    "     "     "   "    "
plot(x,y,'o',xex,yex,'r-');
set(gca,'TickDir','out');   %  Put tick marks outside of axis
hold off
