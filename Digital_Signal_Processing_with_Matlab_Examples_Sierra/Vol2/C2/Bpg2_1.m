% Haar wavelet transform of a data set

y=[3,7,1,15,2,3,6,9]; %data set
Ns=8; %number of samples
K=3; %exponent, 8=2^3
wty=y; %space for the wavelet transform
d=zeros(K,Ns/2); %space for d(j,k) coefficients


for n=1:K,
   aux1= wty(1:2:Ns-1) + wty(2:2:Ns);
   aux2= wty(1:2:Ns-1) - wty(2:2:Ns);
   wty(1:Ns)=[aux1,aux2]/sqrt(2);
   d(K+1-n,1:Ns/2)=wty(1+(Ns/2):Ns); %fill d(j,k) coefficients
   Ns=Ns/2;
end;

subplot(4,1,1)
stem(d(1,1),'k'); hold on; %the d(0,k) coefficients
axis([0 1 0 3]); ylabel('d(0,k)');
title('Example of Haar wavelet transform');

subplot(4,1,2)
stem(d(2,1:2),'k'); hold on; %the d(1,k) coefficients
axis([0 2 -7 0]); ylabel('d(1,k)');

subplot(4,1,3)
stem(d(3,1:4),'k'); hold on; %the d(2,k) coefficients
axis([0 4 -12 0]); ylabel('d(2,k)');

subplot(4,1,4)
stem(y,'k'); hold on; %the data
axis([0 8 0 18]);
ylabel('the data');

d
wty


