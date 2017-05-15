% recover data set from Haar wavelet transform

%the wavelet transform data:
wty=[16.2635,2.1213,-3.0000,-5.0000,-2.8284,-9.8995,-0.7071,-2.1213];

K=3; %exponent, 8=2^3, for 8 samples
J=K+1; %to adapt to MATLAB indexing
y=wty; %space for the recovered data
a=zeros(J,(2^K)); %space for coefficients

m=1;
a(1,1)=y(1);
for n=1:K,
   a(n+1,1:2:(2*m-1))=(a(n,1:m)+y((1+m):(2*m)))/sqrt(2);
   a(n+1,2:2:(2*m))=(a(n,1:m)-y((1+m):(2*m)))/sqrt(2);
   m=m*2;
end;
y=a(4,1:8); %the recovered data

subplot(4,1,1)
stem(a(1,1),'k'); hold on; %the a(0,k) coefficients
axis([0 1 0 20]); ylabel('a(0,k)');
title('Example of data recovery');

subplot(4,1,2)
stem(a(2,1:2),'k'); hold on; %the a(1,k) coefficients
axis([0 2 0 15]); ylabel('a(1,k)');

subplot(4,1,3)
stem(a(3,1:4),'k'); hold on; %the a(2,k) coefficients
axis([0 4 0 15]); ylabel('a(2,k)');

subplot(4,1,4)
stem(y,'k'); hold on; %the data
axis([0 8 0 18]);
ylabel('the data');

a