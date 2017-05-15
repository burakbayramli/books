% composite signal and cepstrum

Td=0.6; %time delay in seconds

%input signal
fs=30; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
tu=0:tiv:(39-tiv); %time intervals set (39 seconds)
Nu=length(tu); %number of data points
u=randn(Nu,1); %random input signal data set

[fnum,fden]=butter(2,0.4); %low-pass filter
ur=filtfilt(fnum,fden,u); %noise filtering

%echo signal
NTd=Td*fs; %number of samples along Td
Ny=Nu+NTd; 
yr=zeros(1,Ny);
yr((NTd+1):Ny)=ur(1:Nu); %y is u delayed Td seconds

%signal adding
z=ur+(0.7*yr(1:Nu))';

ifr=fs/Nu; %frequency interval
fr=0:ifr:((fs/2)-ifr); %frequencies data set

subplot(2,1,1)
plot(tu,z,'k'); %composite signal
title('composite signal and its cepstrum'); ylabel('composite signal');

subplot(2,1,2)
cz=rceps(z); %real cepstrum 
plot(tu(1:(Nu/2)),cz(1:(Nu/2)),'k'); %plots cepstrum
ylabel('real cepstrum'); xlabel('seconds');



