% Geometric PDF
P=0.5;
N=10;
ypdf=zeros(1,N);

for nn=0:N, 
   ypdf(nn+1)=geopdf(nn,P);
end;

stem(0:N,ypdf,'k'); %plots figure
axis([-1 10 0 0.6]);
xlabel('values'); title('Geometric PDF');

