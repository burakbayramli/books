%fixpt.m  to solve x=g(x).
function [x,err,xx]= fixpt(g,x0,TolX,MaxIter)
%input:    g=  ftn to be given as a string 'g'
%         x0= the initial guess
%       TolX= the limit of incremental difference |x(n+1)-x(n)|
%    MaxIter= the maximum # of iteration
%output:   x= the point which the algorithm has reached
%        err= the last value |x(k)-x(k-1)| achieved
%         xx= the history of x
EPS=1e-6;
if nargin<4, MaxIter=100; end 
if nargin<3, TolX=EPS; end 
xx(1)=x0; 
for k=2: MaxIter
   xx(k)= feval(g, xx(k-1));
   err= abs(xx(k)-xx(k-1));
   if err<TolX, break;  end
end
x=xx(k);
if k==MaxIter 
  fprintf('Do not rely on this solution, even if it is the best I could do in %d iterations\n',MaxIter) 
end
