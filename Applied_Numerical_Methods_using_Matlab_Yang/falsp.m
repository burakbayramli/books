%falsp.m  to solve f(x)=0.
function [x,err,xx]= falsp(f,a,b,TolX,MaxIter)
%input:    f=  ftn to be given as a string 'f'
%          a/b= the initial left/right point of the solution interval
%          TolX= the upper limit of  max(|x(k)-a|,|b-x(k)|)
%          MaxIter= the maximum # of iteration
%output: x= the point which the algorithm has reached
%          err= max(|x(last)-a|,|b-x(last)|)
%          xx= the history of x
TolFun=eps; EPS=1e-6;
if nargin<5, MaxIter=100; end
if nargin<4, TolX=EPS; end
fa=feval(f,a);  fb=feval(f,b);
if fa*fb>0
   error('We must have f(a)f(b)<0!')
end
for k=1: MaxIter
   %fa=feval(f,a);  fb=feval(f,b);
   xx(k)= (a*fb-b*fa)/(fb-fa); %Eq.(4.3-1)
   fx= feval(f,xx(k)); err= max(abs(xx(k)-a),abs(b-xx(k)));
   if (abs(fx)<TolFun)|(err<TolX), break; 
    elseif fx*fa>0,  a=xx(k); fa=fx;
    else  b=xx(k); fb=fx;
   end
end
x= xx(k);
if k==MaxIter
  fprintf('Do not rely on this, though the best in %d iterations\n',MaxIter) 
end

