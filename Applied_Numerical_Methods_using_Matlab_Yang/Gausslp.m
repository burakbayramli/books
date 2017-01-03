function [t,w]=Gausslp(N)
if N<0, error('Gauss-Legendre polynomial of negative order?'); end 
t=roots(Lgndrp(N))'; %make it a row vector
A(1,:)= ones(1,N);  b(1)=2;
for n=2:N
   A(n,:)=A(n-1,:).*t; 
   if mod(n,2)==0, b(n)=0;
     else  b(n)=2/n;
   end
end
w= b/A'; 