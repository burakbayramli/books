%Obtain  discrete transfer function (DTrF) from discrete frequency response to noise
%DTrF case 1
% continuous time transfer function:
num1Ns=10; den1Ns=[1 10]; %G1(s)=10/(s+10);
fs=400; %sampling frequency in Hz
%discrete transfer function (from the continuous case)
[num1Nz,den1Nz]= impinvar(num1Ns,den1Ns,fs); %G1(z)
%discrete frequency response of G1(z)
wr=logspace(-1,2); %frequency values for response (rad/s)
H1Nz=freqz(num1Nz,den1Nz,wr/(2*pi),fs); %G1(z) frequency response

%response of G1 to noise
tiv=1/fs; %time interval between samples;
t=0:tiv:(60-tiv); %time intervals set (60 seconds)
N=length(t); %number of data points
u=randn(N,1); %random input signal data set
y=filter(num1Nz,den1Nz,u); %G1(z) response to noise

%display of input and output signals
figure(1)
subplot(2,1,1)
plot(t,u,'k'); %input u plot
xlabel('seconds'); ylabel('system input');
title('input and output signals: case 1')
subplot(2,1,2)
plot(t,y,'k'); %output y plot
ylabel('system output');
%------------------------------------------------
%frequency response estimate,using tfe
nfft=2048; %length of FFT
window=hanning(nfft); %window function
[H1Rz,F1Rz]=tfe(u(1000:N),y(1000:N),nfft,fs,window); %frequency response estimate

%comparing original and estimated frequency responses
figure(2)
plot(H1Nz,'xr'); hold on;
plot(H1Rz,'k');
x1=-0.2; x2=1.2; y1=-0.6; y2=0.2;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('complex frequency response, H1N(z)as x & H1R(z) solid');
xlabel('real'); ylabel('imaginary');
%------------------------------------------------

%using invfreqz to obtain the DTrF
na=1; %denominator degree
nb=0; %numerator degree
W=F1Rz*2*pi/fs; %normalized frequency 0..pi 
[num1Ez,den1Ez]=invfreqz(H1Rz,W,nb,na); %DTrF computation

H1Ez=freqz(num1Ez,den1Ez,wr/(2*pi),fs); %^G1(z) frequency response

%comparing G1(z) and ^G1(z) frequency responses
figure(3)
plot(H1Nz,'xr'); hold on;
plot(H1Ez,'k');
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('complex frequency response, G1N(z)as x & G1E(z) solid');
xlabel('real'); ylabel('imaginary');

num1Nz
num1Ez

den1Nz
den1Ez
