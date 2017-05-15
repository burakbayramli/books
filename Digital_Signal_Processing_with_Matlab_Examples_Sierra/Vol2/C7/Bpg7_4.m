% Scatterplot example
% 2 variables with some correlation
N=5000;
a=normrnd(0,1,1,N); a=a-mean(a);
b=(0.4*normrnd(0,1,1,N))+(0.6*a); b=b-mean(b);

figure(1)
plot(a,b,'k.'); hold on; %scatterplot
plot([-5 5],[0 0],'k');
plot([0 0],[-5 5],'k');
axis([-5  5 -5 5]);
title('scatterplot: correlated variables'); 
xlabel('a'); ylabel('b');

X=[a; b];	%data matrix: 2 rows of N values
%print covariance matrix
Sx=cov(X') %covariance matrix
