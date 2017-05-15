% coloured noise and echo

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
ty=0:tiv:(39+Td-tiv); %time intervals set (39+Td seconds)

%signal adding
z=ur+(0.7*yr(1:Nu))';

subplot(2,1,1)
plot(tu,ur,'k'); %plots input signal
title('coloured noise signals'); ylabel('input');

subplot(2,1,2)
plot(ty,yr,'k'); %plots echo signal
ylabel('echo signal'); xlabel('seconds');




