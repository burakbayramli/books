% Sum of two overlapping Gaussian PDFs
% and two approximations

x=0:0.1:20;
mu1=5; sigma1=0.9;
yn1=normpdf(x,mu1,sigma1); %normal pdf
mu2=10; sigma2=3;
yn2=2*normpdf(x,mu2,sigma2); %normal pdf
ys=yn1+yn2; %PDF adding

%Approximations
mua=5; sigmaa=0.8;
ya1=1.8*normpdf(x,mua,sigmaa); %normal pdf
mua=8.5; sigmaa=3.6;
ya2=3.2*normpdf(x,mua,sigmaa); %normal pdf

%display
figure(1)
subplot(1,2,1)
plot(x,ys,'k'); hold on;
plot(x,ya1,'r--')
axis([0 18 0 1]);
title('MAP approximation')
xlabel('x'); ylabel('y');

subplot(1,2,2)
plot(x,ys,'k'); hold on;
plot(x,ya2,'r--')
axis([0 18 0 0.6]);
title('VB approximation')
xlabel('x'); ylabel('y');

