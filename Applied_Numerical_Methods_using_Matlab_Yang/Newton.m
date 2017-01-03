%newton.m  to solve f(x)=0.
function [x,fx,xx]= newton(f,x0,TolX,MaxIter,MaxIter1)
%  can be used as   newton(f,dx,x0,  TolX,   MaxIter)
%input:     f= ftn to be solved
%          df= df(x)/dx (If not given, numerical derivative is used.)
%          x0= the initial guess of the solution
%        TolX= the upper limit of |x(k)-x(k-1)|
%     MaxIter= the maximum # of iteration
%output:    x= the point which the algorithm has reached
%          fx= f(x(last))
%          xx= the history of x
h=1e-4; h2= 2*h; TolFun=eps; EPS=1e-6;
if isnumeric(x0)
  if nargin<4, MaxIter=100;
    if nargin<3, TolX=EPS; end
  end
 else
  df=x0; x0=TolX;
  if nargin>3, TolX=MaxIter; else TolX=EPS; end
  if nargin>4, MaxIter=MaxIter1; else MaxIter=100; end
end
xx(1)= x0; fx=feval(f,x0);
for k=1: MaxIter
   if exist('df'),  dfdx= feval(df,xx(k));
    else  dfdx= (feval(f,xx(k)+h)-feval(f,xx(k)-h))/h2;
   end
   dx= -fx/dfdx; xx(k+1)= xx(k)+dx;
   fx= feval(f, xx(k+1));
   if abs(fx)<TolFun|abs(dx)<TolX, break; end
end
x= xx(k+1);
if k==MaxIter
  fprintf('Do not rely on this, though the best in %d iterations\n',MaxIter) 
end
