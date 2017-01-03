function count=sturmcount(A,x)

n=size(A,1);
p=[0,1,A(1,1)-x];
for k=2:n
  p=[p,(A(k,k)-x)*p(end)-A(k,k-1)^2*p(end-1)];
end

count=sum(diff([1,sign(p(3:end))])~=0);
