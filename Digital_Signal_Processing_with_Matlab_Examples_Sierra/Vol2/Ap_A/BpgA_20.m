%Obtain  discrete transfer function (DTrF) from discrete frequency response to noise
%DTrF case 2
% continuous time transfer function:
num2Ns=[10 0]; den2Ns=[1 3 10]; %G2(s)=10s/(s^2+3s+10);
fs=300; %sampling frequency in Hz
%discrete transfer function (from the continuous case)
[num2Nz,den2Nz]= impinvar(num2Ns,den2Ns,fs); %G2(z)
%discrete frequency response of G2(z)
wr=logspace(-1,2); %frequency values for response (rad/s)
H2Nz=freqz(num2Nz,den2Nz,wr/(2*pi),fs); %G2(z) frequency response

%response of G2 to noise
tiv=1/fs; %time interval between samples;
t=0:tiv:(60-tiv); %time intervals set (60 seconds)
N=length(t); %number of data points
u=randn(N,1); %random input signal data set
y=filter(num2Nz,den2Nz,u); %G2(z) response to noise

%display of input and output signals
figure(1)
subplot(2,1,1)
plot(t,u,'k'); %input u plot
xlabel('seconds'); ylabel('system input');
title('input and output signals: case 2')
subplot(2,1,2)
plot(t,y,'k'); %output y plot
ylabel('system output');

%------------------------------------------------
%frequency response estimate,using tfe
nfft=4096; %length of FFT
window=hanning(nfft); %window function
[H2Rz,F2Rz]=tfe(u(1000:N),y(1000:N),nfft,fs,window); %frequency response estimate

%comparing original and estimated frequency responses
figure(2)
plot(H2Nz,'xr'); hold on;
plot(H2Rz,'k');
x1=-1; x2=4; y1=-2; y2=2;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('complex frequency response, H2N(z)as x & H2R(z) solid');
xlabel('real'); ylabel('imaginary');

%------------------------------------------------

%using invfreqz to obtain the DTrF
na=2; %denominator degree
nb=1; %numerator degree
W=F2Rz*2*pi/fs; %normalized frequency 0..pi 
[num2Ez,den2Ez]=invfreqz(H2Rz,W,nb,na); %DTrF computation

H2Ez=freqz(num2Ez,den2Ez,wr/(2*pi),fs); %^G2(z) frequency response

%comparing G2(z) and ^G2(z) frequency responses
figure(3)
plot(H2Nz,'xr'); hold on;
plot(H2Ez,'k');
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('complex frequency response, G2N(z)as x & G2E(z) solid');
xlabel('real'); ylabel('imaginary');

num2Nz
num2Ez

den2Nz
den2Ez
