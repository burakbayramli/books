%Scatterplots of two sources, and two mixed signals
% the sources are uniformly random variables

%two uniformly random sources,
N=2000;
s1=rand(1,N); s1=2*(s1-mean(s1)); %zero-mean
s2=rand(1,N); s2=2*(s2-mean(s2)); %"  "  "
vr1=var(s1); s1=s1/sqrt(vr1); %variance=1
vr2=var(s2); s2=s2/sqrt(vr2); %"  "  "

s=[s1;s2]; %combine sources

N=length(s1); 
M=[0.2 0.8; 0.4 0.6]; %example of mixing matrix
x=M*s; %mixed signals

%scatterplot of sources
figure(1)
plot(s(1,:),s(2,:),'k.'); 
axis([-2.5 2.5 -2.5 2.5]);
title('scatterplot of two uniformly random sources');
xlabel('s1'); ylabel('s2');

%scatterplot of observed data (the mix)
figure(2)
plot(x(1,:),x(2,:),'k.'); 
axis([-2.5 2.5 -2.5 2.5]);
title('scatterplot of observed data (the mix)');
xlabel('x1'); ylabel('x2');

%print covariance matrix of sources
Ss=cov(s')

%print covariance matrix of observed signals
Sx=cov(x')

