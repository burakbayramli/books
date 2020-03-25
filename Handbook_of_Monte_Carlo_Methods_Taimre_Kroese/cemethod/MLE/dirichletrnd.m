function out=dirichletrnd(a,n,K)
out=zeros(n,K);
for k=1:n
  temp=zeros(1,K);
  for i=1:(K)
	temp(i)=gamrnd(a(i),1);
  end
  out(k,:)=temp./sum(temp);
end