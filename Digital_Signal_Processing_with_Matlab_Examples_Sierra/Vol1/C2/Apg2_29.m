% Mixture of 2 Gaussians

v=-6:0.02:10; %value set
mu1=0; sigma1=1.5; %parameters of Gaussian 1
mu2=5; sigma2=1; %  "  "  "     Gaussian 2
ypdf1=normpdf(v,mu1,sigma1); %PDF1
ypdf2=normpdf(v,mu2,sigma2); %PDF2

p=0.4; %mix parameter

%mixed Gaussian PDF
ypdf=(p*ypdf1)+((1-p)*ypdf2);

%random data generation
N=5000;
y=zeros(1,N); %reserve space
for nn=1:N,
   r=rand(1); %uniform PDF
   if r<p,
      y(nn)=mu1+(sigma1*randn(1)); %PDF1
   else   
      y(nn)=mu2+(sigma2*randn(1)); %PDF2
   end;   
end;     

%histogram normalization
nB=100; %number of bins
h=16/100; %bin width
k=N*h;

%display
figure(1)
[nh,xh]=hist(y,100);
plot(xh,nh/k,'k'); hold on; %density histogram
plot(v,ypdf,'r'); %multi-modal PDF
xlabel('values'); title('Mix of 2 Gaussians: histogram and PDF');
