% Comparison of variogram and covariance

x=0:1:1000;
N=length(x);
C0=0.8;
C=C0*exp(-0.005*x); %covariance model
V=C0-C; %variogram
xC0=C0*ones(1,N); %for horizontal plot

%display
plot(x,xC0,'b--'); hold on;
plot(x,C,'r--');
plot(x,V,'k');
axis([0 1000 0 0.9]);
title('Variogram and Covariance');
xlabel('m');
