%Example of sigma points
%
x1=-10:0.1:10;

%1D Gaussian---------------------------------
sig=3; 
mu=0;
aux=(x1-mu).^2;
y1=(exp(-aux/(2*(sig^2)))/(sig*sqrt(2*pi)));

%sigma points
x1s0=mu;
x1s1=mu+sig;
x1s2=mu-sig;
%the points on PDF
y1s0=max(y1); %the PDF peak
aux=(x1s1-mu).^2;
y1s1=(exp(-aux/(2*(sig^2)))/(sig*sqrt(2*pi)));
aux=(x1s2-mu).^2;
y1s2=(exp(-aux/(2*(sig^2)))/(sig*sqrt(2*pi)));

figure(1)
plot(x1,y1,'k'); hold on; %the PDF

plot(x1s0,0,'rx','MarkerSize',12); %central sigma point
plot(x1s1,0,'rx','MarkerSize',12); %right sigma point
plot(x1s2,0,'rx','MarkerSize',12); %left sigma point

plot([x1s0 x1s0],[0 y1s0],'b--'); %central sigma point line
plot([x1s1 x1s1],[0 y1s1],'b--'); %right sigma point line
plot([x1s2 x1s2],[0 y1s2],'b--'); %left sigma point line
title('sigma points');
axis([-10 10 0 0.15]);