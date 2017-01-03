function [num,den]=padeap(f,xo,M,N,x0,xf)
%Input : f=function to be approximated
%Output: num=numerator coefficients of Pade approximation of order M
%        den=denominator coefficients of Pade approximation of order N
if isstr(f)==0&length(f)>1, a=f;
 else
  a(1)=feval(f,xo);
  h=.005; tmp=1;
  for i=1:M+N
    tmp=tmp*i*h; %i!(factorial i)*h^i;
    c=difapx(i,[-i i]); %coefficient of numerical derivative
    dix=c*feval(f,xo+[-i:i]*h)'; %/h^i; %derivative
    a(i+1)=dix/tmp; %Taylor series coefficient
  end
end
a
%syms x; sym2poly(taylor(1/(1+25*x^2),M+N))
for m=1: N
   n=1:N; A(m,n)=a(M+1+m-n);
   b(m)=-a(M+1+m);
end
d=A\b'; %Eq.(3.4-4b)
%q =zeros(1,M);
%for m=1: M+1
%   q(m) =a(m);
%   for n=1: min(m-1,N)
%      q(m) =q(m) +a(m-n)*d(n);
%   end
%end
for m=1: M+1
   mm= min(m-1,N);
   q(m) =a(m:-1:m-mm)*[1; d(1:mm)]; %Eq.(3.4-4a)
end
num =q(M+1:-1:1)/d(N); %descending order
den =[d(N:-1:1)' 1]/d(N); %descending order
if nargout==0 % plot the true ftn, Pade ftn and Taylor expansion
   if nargin<6, x0=xo-1; xf=xo+1; end
   x =x0+[xf-x0]/100*[0:100];
   yt =feval(f,x);
   x1=x-xo;
   yp=polyval(num,x1)./polyval(den,x1);
   yT=polyval(a(M+N+1:-1:1),x1);
   clf, plot(x,yt,'k', x,yp,'r'), hold on
   pause, plot(x,yT,'b') %Taylor series
end
