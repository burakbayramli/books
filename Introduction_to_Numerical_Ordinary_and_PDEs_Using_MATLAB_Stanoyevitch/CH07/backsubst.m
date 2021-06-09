function x=backsubst(U,b)
%Solves the upper triangular system Ux=b by back substitution
%Inputs:  U = upper triangular matrix,  b = column vector of same dimension
%Output:  x = column vector (solution)
[n m]=size(U);
x(n)=b(n)/U(n,n);
for j=n-1:-1:1
   x(j)=(b(j)-U(j,j+1:n)*x(j+1:n)')/U(j,j);
end
x=x';
