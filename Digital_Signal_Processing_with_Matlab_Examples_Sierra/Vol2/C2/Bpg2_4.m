% Haar wavelet transform of a signal
% Sawtooth signal 
% Plot of wty
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

%figure
plot(wty,'k');
axis([0 256 -4 4]);
title('Vector wty obtained by sawtooth Haar wavelet transform')