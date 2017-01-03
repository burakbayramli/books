function x=backsubst(U,B);
%backward substitution for a upper-triangular matrix eqation Ux=B
N=min(size(U));
x(N,:)= B(N,:)/U(N,N); 
for m=N-1: -1:1
   x(m,:)= (B(m,:)-U(m,m+1:N)*x(m+1:N,:))/U(m,m); 
end
