function [W,R]=house(A)

[m,n]=size(A);
W=zeros(m,n);
for k=1:n
  v=A(k:m,k);
  v(1)=v(1)+(2*(v(1)>=0)-1)*norm(v);
  v=v/norm(v);
  W(k:m,k)=v;
  A(k:m,k:n)=A(k:m,k:n)-2*v*(v'*A(k:m,k:n));
end
R=triu(A(1:n,1:n));
