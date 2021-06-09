function x=fwdsubst(L,b)
%Solves the lower triangular system Lx=b by forward substitution
%Inputs:  L = lower triangular matrix,  b = column vector of same dimension
%Output:  x = column vector (solution)
[n m]=size(L);
x(1)=b(1)/L(1,1);
for j=2:n
   x(j)=(b(j)-L(j,1:j-1)*x(1:j-1)')/L(j,j);
end
x=x';
