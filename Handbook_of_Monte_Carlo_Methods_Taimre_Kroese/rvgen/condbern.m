function sample = condbern(k,p)
w=zeros(1,length(p));
sample=zeros(1,k);
ind1=find(p==1);
sample(1:length(ind1))=ind1;
k=k-length(ind1);
ind=find(p<1 & p>0);
w(ind)=p(ind)./(1-p(ind));
for i=1:k
    a=zeros(1,length(ind));
    Rvals=Rgens(k-i+1,w(ind));
    for j=1:length(ind)
        a(j)=w(ind(j))*Rvals(j+1)/((k-i+1)*Rvals(1));
    end
    a=cumsum(a);
    entry=ind(min(find(a>rand)));
    ind=ind(find(ind~=entry));
    sample(length(ind1)+i)=entry;
end
sample=sort(sample);

