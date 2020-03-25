function out=dirichlet_log_like(data,x,n,K)
out=zeros(size(x,1),1);
I=any(x<=0,2);nI=~I;
out(I)=-inf;
out(nI)=n.*(log(gamma(sum(x(nI,:),2)))-sum(log(gamma(x(nI,:))),2));
for k=1:n
    out(nI)=out(nI)+sum((x(nI,1:(K-1))-1).*...
        repmat(log(data(k,1:(K-1))),sum(nI),1),2)+(x(nI,K)-1).*...
        repmat(log(1-sum(data(k,1:(K-1)),2)),sum(nI),1);
end