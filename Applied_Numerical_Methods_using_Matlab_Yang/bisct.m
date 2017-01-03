%bisct.m  to solve f(x)=0.
function [x,err,xx]= bisct(f,a,b,TolX,MaxIter)
%input:  f=  ftn to be given as a string 'f'
%        a/b= the initial left/right point of the solution interval
%        TolX= the upper limit of incremental difference |x(n+1)-x(n)|
%        MaxIter= the maximum # of iteration
%output: x= the point which the algorithm has reached
%        err= (b-a)/2(the last interval's)
%        xx= the history of x
TolFun=eps; EPS=1e-6;
if nargin<5, MaxIter=100; end 
if nargin<4, TolX=EPS; end 
fa=feval(f,a); fb=feval(f,b); 
if fa*fb>0
  error('We must have f(a)f(b)<0!')
  k=1; xx(k)=(a+b)/2; err=99;
 else
   for k=1: MaxIter
     xx(k)= (a+b)/2;
     fx= feval(f, xx(k));   err= (b-a)/2;
     if (abs(fx)<TolFun)|(abs(err)<TolX), break; 
      elseif fx*fa>0,  a=xx(k); fa=fx;
      else  b=xx(k); fb=fx;
     end
   end
end
x= xx(k);
if k==MaxIter 
  fprintf('Do not rely on this, though the best in %d iterations\n',MaxIter) 
end
