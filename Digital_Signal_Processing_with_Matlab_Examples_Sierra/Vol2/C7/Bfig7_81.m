% Two separated Gaussian PDFs
% and 3 approximations

x=0:0.1:20;
mu1=4; sigma1=0.9;
yn1=normpdf(x,mu1,sigma1); %normal pdf
mu2=13; sigma2=1.8;
yn2=normpdf(x,mu2,sigma2); %normal pdf
ys=yn1+yn2; %PDF adding

%approximations
mua=4; sigmaa=0.8;
ya1=1.6*normpdf(x,mua,sigmaa); %normal pdf

mua=13; sigmaa=1.5;
ya2=1.9*normpdf(x,mua,sigmaa); %normal pdf

mua=8.5; sigmaa=5;
ya3=1.4*normpdf(x,mua,sigmaa); %normal pdf


%display
figure(1)
subplot(3,1,1)
plot(x,ys,'k'); hold on;
plot(x,ya1,'r--');
axis([0 20 0 0.9]);
title('Approximation on mode 1')
ylabel('y');

subplot(3,1,2)
plot(x,ys,'k'); hold on;
plot(x,ya2,'r--');
axis([0 20 0 0.9]);
title('Approximation on mode 2')
ylabel('y');

subplot(3,1,3)
plot(x,ys,'k'); hold on;
plot(x,ya3,'r--');
axis([0 20 0 0.9]);
title('Moment matched approximation')
xlabel('x'); ylabel('y');

