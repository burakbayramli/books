function I = plotSimpInt(fun,a,b,nsub)
% plotSimpInt  Graphical display of composite Simpson rule integration
%
% Synopsis:   I = plotSimpInt(fun,a,b)
%             I = plotSimpInt(fun,a,b,nsub)
%
% Input:     fun  = (string) name of m-file that evaluates f(x)
%            a,b  = lower and upper limits of the integral
%            nsub = number of subintervals to use in the integration
%                   Default: nsub=4
%
% Output:     I = integral of 'fun(x)' from a to b
%             In addition a plot of the trapezoid areas that add up
%             to the integral is shown

if nargin<4,  nsub=4;  end

% --- evaluate integral
n = 2*nsub + 1;      %  total number of points
h = (b-a)/(n-1);     %  stepsize
x = a:h:b;           %  divide the interval
y = feval(fun,x);    %  evaluate integrand;  y is a row vector

I = (h/3)*( y(1) + 4*sum(y(2:2:n-1)) + 2*sum(y(3:2:n-1)) + y(n) );
% = (h/3)*( f(a) + 4*f_even          + 2*f_odd           + f(b) );

% --- Plot filled boxes, then overlay f(x)
hold on
kk = 0;                     %  kk is counter, k is always odd
for k=1:2:length(x)-1;
  kk = kk + 1;
  fillQuadBox([x(k); x(k+1); x(k+2)],[y(k); y(k+1); y(k+2)],25,kk);
end

xex = linspace(a,b);        %  x values for smooth curve of exact f(x)
yex = feval(fun,xex);       %  y values  "    "     "     "   "    "
plot(x,y,'o',xex,yex,'r-');
set(gca,'TickDir','out');   %  Put tick marks outside of axis
hold off

% ------------- 
function fillQuadBox(xb,yb,m,ktime)
% fillQuadBox  Fills a "box" defined by a quadratic surface at the top
%
% Synopsis:  fillQuadBox(xb,yb)
%            fillQuadBox(xb,yb,m)
%            fillQuadBox(xb,yb,m,ktime)
%
% Input:     xb,yb = vectors, length 3, defining (x,y) points of the
%                    quadratic curve
%            m = (optional) number of points used to define the top
%                surface.  Default:  m = 25
%            ktime = (optional) index of the number of times fillQuadBox
%                    has been called.  Workaround for lack of static
%                    variables in MATLAB.  Color of the "box" is dark
%                    if ktime is odd or light if ktime is even.
%                    Default:  ktime=1  (dark box)
%
% Output:    Draw a filled box in current figure window

if nargin<3,  m=25;     end
if nargin<4,  ktime=1;  end

% --- build a curved polygon with m points along the curved top surface
c = vander(xb(:))\yb(:);        %  coefficients of quadratic polynomial
ib = 2;  ie = (m-1) + 2;        %  indices at begin/end of quad surface
xp = zeros(m+3,1);  yp = xp;
xp(1) = xb(1);  xp(ie+1) = xb(3);  xp(ie+2) = xb(1);
yp(1) = 0;      yp(ie+1) = 0;      yp(ie+2) = 0;
xp(ib:ie) = linspace(xb(1),xb(3),m);
yp(ib:ie) = polyval(c,xp(ib:ie));   

colr = [0.8  0.8  1; ...   %  colr contains two RGB color sets
        0.92 0.92 1];      %  1st row is darker blue, 2nd row is lighter
ic = 1 + rem(ktime,2);     %  ic=1 if k is even, ic=2 if k is odd

fill(xp,yp,colr(ic,:));    %  Fill the polygon shaped box

