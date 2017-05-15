% Haar wavelet transform of a signal
% Sawtooth signal recovery from transform

%---------------------------------------------------
% First the wavelet transform to get data
fy=300; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

fs=20000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
Ns=256; %let us take 256 signal samples
duy=Ns*tiv; %time for 256 samples
t=0:tiv:(duy-tiv); %time intervals set

y=sawtooth(wy*t); %signal data set (256 samples)

K=8; %exponent, 256=2^8
wty=y; %space for the wavelet transform

%the Haar wavelet transform
for n=1:K,
   aux1= wty(1:2:Ns-1) + wty(2:2:Ns);
   aux2= wty(1:2:Ns-1) - wty(2:2:Ns);
   wty(1:Ns)=[aux1,aux2]/sqrt(2);
   Ns=Ns/2;
end;

%------------------------------------------------------------
% Second the signal recovery from the wavelet transform data

J=K+1;
z=wty; %space for recovered data
a=zeros(J,(2^K)); %space for a(j,k) coefficients

m=1;
a(1,1)=z(1);
for n=1:K,
   a(n+1,1:2:(2*m-1))=(a(n,1:m)+z((1+m):(2*m)))/sqrt(2);
   a(n+1,2:2:(2*m))=(a(n,1:m)-z((1+m):(2*m)))/sqrt(2);
   m=m*2;
end;
y=a(J,1:256); %the recovered data

%figure
plot(y,'k');
axis([0 256 -1.2 1.2]);
title('Recovered sawtooth signal, from Haar wavelet transform');

