% noise and pure time delay
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

%plot input and output
subplot(2,1,1)
plot(tu,u,'k'); %plots input
axis([0 70 -4 4]);
title('noise signal u')
subplot(2,1,2)
plot(ty,y,'k'); %plots output (has delay)
axis([0 70 -4 4]);
title('noise signal y, which is delayed u');
xlabel('seconds');
