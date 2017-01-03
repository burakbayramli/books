function [t,w]=Gausshp(N)
if N<0, error('Gauss-Hermite polynomial of negative order??'); end 
t=roots(Hermitp(N))';
A(1,:)= ones(1,N);  b(1)=sqrt(pi);
for n=2:N
   A(n,:)=A(n-1,:).*t;
   if mod(n,2)==1, b(n)= (n-2)/2*b(n-2);
     else   b(n)= 0; 
   end
end
w= b/A'; 