%7.4  arnoldicode.m

n=6;
A=randn(n,n);
b=randn(n,1);

Q=zeros(n,n);
H=zeros(n,n-1);

Q(:,1)=b/norm(b);
for j=1:n-1
   t=A*Q(:,j);
   for i=1:j
     H(i,j)=Q(:,i)'*t;
     t=t-H(i,j)*Q(:,i);
   end
   H(j+1,j)=norm(t);
   Q(:,j+1)=t/H(j+1,j);
end
