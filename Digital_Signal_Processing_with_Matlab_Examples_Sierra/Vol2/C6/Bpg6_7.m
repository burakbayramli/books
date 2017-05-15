% Cross correlation in noise and pure time delay
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
ty=0:tiv:(60+Td-tiv); %time intervals set (60+Td seconds)

ac=xcorr(u,y); %cross-correlation of u and y
ac=ac/Ny; %normalization
Nac=length(ac); mNac=ceil(0.5*Nac); %to plot half ac
plot(ty,ac(mNac:Nac),'k');
title('cross-correlation'); xlabel('seconds');

[V K]=max(ac);
eNTd=K-mNac;
eTd=eNTd/fs



