function [xo,fo]=opt_conjg(f,x0,TolX,TolFun,alpha0,MaxIter,KC)
%KC=1: Polak-Ribiere Conjugate Gradient method
%KC=2: Fletcher-Reeves Conjugate Gradient method
if nargin<7, KC=0; end
if nargin<6, MaxIter=100; end
if nargin<5, alpha0=10; end
if nargin<4, TolFun=1e-8; end
if nargin<3, TolX=1e-6; end
N=length(x0); nmax1=20; warning=0; h=1e-4; %dimension of variable
x=x0;  fx=feval(f,x0);  fx0=fx;
for k=1: MaxIter
  xk0= x; fk0= fx; alpha= alpha0;
  g= grad(f,x,h);  s= -g;
  for n=1:N                      
     alpha= alpha0; 
     fx1= feval(f,x+alpha*2*s); %trial move in search direction
     for n1=1:nmax1 %To find the optimum step-size by line search
        fx2= fx1; fx1= feval(f,x+alpha*s); 
        if fx0>fx1+TolFun & fx1<fx2-TolFun %fx0>fx1<fx2
          den=4*fx1-2*fx0-2*fx2; num=den-fx0+fx2; %Eq.(7.1-5)
          alpha= alpha*num/den;  
          x= x+alpha*s;  fx= feval(f,x);  
          break;
         elseif n1==nmax1/2
          alpha= -alpha0;  fx1= feval(f,x+alpha*2*s);
         else
          alpha= alpha/2;         
        end
     end
     x0= x;  fx0= fx;
     if n<N
       g1= grad(f,x,h); 
       if KC<=1,  s= -g1 +(g1-g)*g1'/(g*g'+1e-5)*s; %(7.1-20a) 
        else  s= -g1 +g1*g1'/(g*g'+1e-5)*s; %(7.1-20b)
       end
       g= g1; 
     end
     if n1>=nmax1, warning=warning+1; %can't find optimum step-size
      else  warning= 0;
     end        
  end
  if warning>=2|(norm(x-xk0)<TolX&abs(fx-fk0)<TolFun), break;  end
end    
xo= x;  fo= fx;
if k==MaxIter, fprintf('Just best in %d iterations',MaxIter), end
