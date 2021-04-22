function x = thomas(a,d,b,c)
%solves matrix equation Ax=c, where A is a tridiagonal matrix
%Inputs:  a=upper diagonal of matrix A a(n)=0, d=diagonal of A,
%b=lower diagonal of A, b(1)=0, c=right-hand side of equation
n=length(d);
a(1)=a(1)/d(1);
c(1)=c(1)/d(1);
for i=2:n-1
   denom=d(i)-b(i)*a(i-1);
   if (denom==0), error('zero in denominator'), end
   a(i)=a(i)/denom;
   c(i)=(c(i)-b(i)*c(i-1))/denom;
end
c(n)=(c(n)-b(n)*c(n-1))/(d(n)-b(n)*a(n-1));
x(n)=c(n);
for i=n-1:-1:1
   x(i)=c(i)-a(i)*x(i+1);
end
