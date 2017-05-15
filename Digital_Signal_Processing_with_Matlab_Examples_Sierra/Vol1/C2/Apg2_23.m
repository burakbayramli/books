% Binomial PDF
n=20;
ypdf=zeros(1,n);

for k=1:n,
ypdf(k)=binomial(n,k);
end;

stem(ypdf,'k'); %plots figure
xlabel('values'); title('Binomial PDF');

