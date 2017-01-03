function g= jacob1(f,x,h,varargin) %Jacobian of f(x)
if nargin<3, h=.0001; end
h2=2*h; N=length(x); I= eye(N); 
for n=1:N
 if abs(x(n))<.0001, x(n)=.0001; end
   delta=h*x(n);
   tmp=I(n,:)*delta;
   f1=feval(f,x+tmp,varargin{:});
   f2=feval(f,x-tmp,varargin{:});
   f12=(f1-f2)/2/delta; g(:,n)=f12(:);
end
