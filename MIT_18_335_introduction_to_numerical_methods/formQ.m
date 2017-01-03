function Q=formQ(W)

[m,n]=size(W);
Q=eye(m);
for k=n:-1:1
  Q(k:m,:)=Q(k:m,:)-2*W(k:m,k)*(W(k:m,k)'*Q(k:m,:));
end
