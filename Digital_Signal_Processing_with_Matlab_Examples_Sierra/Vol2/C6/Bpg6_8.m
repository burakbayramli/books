% Frequency response estimation in noise and pure time delay
Td=2; %time delay in seconds
%input signal
fs=20; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
tu=0:tiv:(60-tiv); %time intervals set (60 seconds)
Nu=length(tu); %number of data points
u=randn(Nu,1); %random input signal data set
%output signal
NTd=Td*fs; %number of samples along Td
Ny=Nu+NTd; 
y=zeros(1,Ny);
y((NTd+1):Ny)=u(1:Nu); %y is u delayed Td seconds

nfft=256; %length of FFT
window=hanning(256); %window function
[GE,FE]=tfe(u(61:Nu),y(61:Nu),nfft,fs,window); %transfer function estimate with stable data
phased=unwrap(angle(GE));
w=FE*2*pi; %frequency in rad/s
plot(w,phased,'k'); %plots phase of estimated frequency response of the system
title('estimation of the phase caused by the delay')
xlabel('rad/s'),; ylabel('rad');

dtangent=(phased(100)-phased(10))/(w(100)-w(10)); %taking last part of the phase line
eTd=-dtangent; %Td estimate
eTd %result display


