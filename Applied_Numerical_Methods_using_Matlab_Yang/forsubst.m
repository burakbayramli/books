function x=forsubst(L,B);
%forward substitution for a lower-triangular matrix eqation Lx=B
N=min(size(L));
x(1,:)= B(1,:)/L(1,1); 
for m=2:N
   x(m,:)= (B(m,:)-L(m,1:m-1)*x(1:m-1,:))/L(m,m); 
end
