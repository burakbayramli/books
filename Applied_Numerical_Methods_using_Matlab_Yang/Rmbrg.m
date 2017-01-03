function [x,R,err,N]=rmbrg(f,a,b,tol,K,varargin)
% construct Romberg table to find the definite integral of f over [a,b]
EPS=1e-12;
if nargin<5, K=10; end
h=b-a; N=1; 
fa=feval(f,a,varargin{:}); 
if isnan(fa)|abs(fa)==inf
  a=a+max(abs(a)*EPS,EPS); fa=feval(f,a,varargin{:}); end
fb=feval(f,b,varargin{:}); 
if isnan(fb)|abs(fb)==inf
  b=b-max(abs(b)*EPS,EPS); fb=feval(f,b,varargin{:});
end
R(1,1)=h/2*(fa+fb);
for k=2:K
   h=h/2;  N=N*2;
   R(k,1)=R(k-1,1)/2 +h*sum(feval(f,a+[1:2:N-1]*h,varargin{:})); %Eq.(5.7-1)
   tmp=1;
   for n=2:k
      tmp= tmp*4;
      R(k,n)= (tmp*R(k,n-1)-R(k-1,n-1))/(tmp-1); %Eq.(5.7-3)
   end
   err= abs(R(k,k-1)-R(k-1,k-1))/(tmp-1); %Eq.(5.7-4)
   if err<tol,  break;  end
end
x=R(k,k);
%fprintf('x=%14.11f\n',x)