% Example of Douglas-Rachford 
% for compressed sensing of 1D signal
clear all;

N=300; %problem dimension
q=N/4; %number of measurements
A=randn(q,N)/sqrt(q); %random Gaussian sensing matrix

% A s-sparse signal, with s non-zero values (=1)
s=13;
x0=zeros(N,1);
aux=randperm(N);
x0(aux(1:s))=1; 

% display
% the signal to be measured
figure(1)
stem(x0,'k');
axis([0 N -0.1 1.1]);
title('the original signal')

% perform random mesurements
b=A*x0;

% Algorithm for recovering the measured signal
niter=100;
lambda=1.0; ; gamma=1.0;
x=zeros(N,1);
y=zeros(N,1);
rnx=zeros(niter,1);

uA=A'*inv(A*A');
for nn=1:niter, 
   % x update
   x=y+(uA*(b-(A*y))); %proxF(indicator function)
   %proxG (soft thresholding):
   aux=(2*x)-y;
   S=max(0,1-gamma./max(0,abs(aux))).*aux; %soft th.
   % y update
   y=y+ (lambda*(S-x));  
   % recording
   rnx(nn)=norm(x,1);
end   

% display
% recovered signal
figure(2)
stem(x,'k');
axis([0 N -0.1 1.1]);
title('recovered signal')

% evolution of ||x||1
figure(3)
plot(rnx,'k');
title('evolution of signal norm-1')
axis([0 100 12 24]);

