% Poisson PDF
lambda=20;
N=50;
ypdf=zeros(1,N);

for nn=1:N,
ypdf(nn)=poisspdf(nn,lambda);
end;

stem(ypdf,'k'); %plots figure
axis([0 N 0 0.1]);
xlabel('values'); title('Poisson PDF');
