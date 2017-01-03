function [xo,fo]= opt_steep(f,x0, TolX,TolFun,alpha0,MaxIter) 
% minimize the ftn f by the steepest descent method.
%input:  f = ftn to be given as a string 'f'
%        x0= the initial guess of the solution
%output: xo= the minimum point reached
%        fo= f(x(o))
if nargin<6, MaxIter=100; end %maximum # of iteration
if nargin<5, alpha0=10; end %initial step size
if nargin<4, TolFun=1e-8; end %|f(x)|<TolFun wanted
if nargin<3, TolX=1e-6; end %|x(k)-x(k-1)|<TolX wanted
x=x0;  fx0= feval(f,x0); fx=fx0;
alpha= alpha0;  kmax1=25; 
warning= 0; %the # of vain wanderings to find the optimum step size
for k=1: MaxIter
g= grad(f,x); g= g/norm(g); %gradient as a row vector
   alpha= alpha*2; %for trial move in negative gradient direction
   fx1 =feval(f,x-alpha*2*g);
for k1=1:kmax1 %find the optimum step size(alpha) by line search
      fx2= fx1; fx1= feval(f,x-alpha*g); 
      if fx0>fx1+TolFun&fx1<fx2-TolFun %fx0>fx1<fx2
den=4*fx1-2*fx0-2*fx2; num=den-fx0+fx2; %Eq.(7.1-5)
alpha= alpha*num/den;   
x= x-alpha*g;  fx= feval(f,x); %Eq.(7.1-9)
break;
      else  alpha= alpha/2;
      end
   end
   if k1>=kmax1, warning=warning+1; %failed to find optimum stepsize
    else  warning= 0;
   end      
   if warning>=2|(norm(x-x0)<TolX&abs(fx-fx0)<TolFun), break;  end
   x0= x;  fx0= fx;
end    
xo= x;  fo= fx;
if k==MaxIter, fprintf('Just best in %d iterations',MaxIter), end
