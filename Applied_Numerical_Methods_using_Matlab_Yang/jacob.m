function g= jacob(f,x,h,varargin) %Jacobian of f(x)
if nargin<3,  h=.0001; end
N= length(x); h2= 2*h; %h12=12*h; 
x=x(:).'; I= eye(N); 
for n=1:N
   f1=feval(f,x+I(n,:)*h,varargin{:});
   f2=feval(f,x-I(n,:)*h,varargin{:});
   %f3=feval(f,x+I(n,:)*h2,varargin{:});
   %f4=feval(f,x-I(n,:)*h2,varargin{:});
   f12=(f1-f2)/h2;
   %f12=(8*(f1-f2)-f3+f4)/h12;
   g(:,n)=f12(:);
end
if sum(sum(isnan(g)))==0&rank(g)<N
    format short e
    fprintf('At x=%12.6e, Jacobian singular with J=',x);
    disp(g); format short; 
end