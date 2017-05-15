% Experimental variogram characteristics

x=0:1:1000;
N=length(x);
C0=0.8;
C=C0*exp(-0.0105*x); %covariance model
V=0.1+C0-C; %variogram


Si=0.9*ones(1,600); %for horizontal plot


%display
plot([1 600],[0.9 0.9],'b--'); hold on;
plot([600 600],[0 0.9],'b--');
plot([0 150],[0.1 0.1],'b--');
%plot(x,C,'r--');
plot(x,V,'k');
axis([0 1000 0 1.1]);
title('Variogram characteristics');
xlabel('m');
