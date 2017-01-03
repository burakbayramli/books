function [Q,H]=arnoldi(A,b,k)

m=size(A,1);
Q=zeros(m,k);
H=zeros(k+1,k);
Q(:,1)=b/norm(b);
for n=1:k
  v=A*Q(:,n);
  for j=1:n
    H(j,n)=Q(:,j)'*v;
    v=v-H(j,n)*Q(:,j);
  end
  H(n+1,n)=norm(v);
  Q(:,n+1)=v/H(n+1,n);
end
