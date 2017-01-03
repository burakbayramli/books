%Program 3.2
function [n,DD]=newtonp(x,y)
%Input : x=[x0 x1 ... xN] 
%         y=[y0 y1 ... yN]
%Output: n=Newton polynomial coefficients of order N
N= length(x)-1;
DD =zeros(N+1,N+1);
DD(1:N+1,1)=y';
for k=2:N+1 
   for m=1: N+2-k
      DD(m,k)=(DD(m+1,k-1)-DD(m,k-1))/(x(m+k-1)-x(m)); %Divided Difference
   end
end
a=DD(1,:); %a_k=DD(1,k) (Eq.(3.10))
n=a(N+1); %Eq.(3.11)
for k=N:-1:1
   n =conv(n,[1 -x(k)])+[zeros(size(n)) a(k)]; %n(x)*(x-x(k-1))+a_k-1
   %n =conv(n,[1 -x(k)]); %n(x)*(x-x(k-1))
   %n(end) =n(end)+DD(1,k); %n(x)<-n(x)*(x-x(k-1))+a_k-1 (Eq.(3.11))
end
