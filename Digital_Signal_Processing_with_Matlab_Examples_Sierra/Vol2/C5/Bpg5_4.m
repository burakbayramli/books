% Cross-correlations
% sine signal + noise
fx=1; %signal frequency in Hz
wx=2*pi*fx; %signal frequency in rad/s

fs=60; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(4-tiv); %time intervals set

x=sin(wx*t); %signal data set
Nx=length(x);
v=randn(1,Nx); %normal noise
y=x+v; %sine+noise

%Cross-correlations
syy=xcorr(y); %symmetrical auto-correlation sequence 
sxy=xcorr(x,y); %symmetrical cross correlation

%display---------------------------

figure(1)
plot(syy,'k'); %auto-correlation
axis([0 479 -200 250]);
title('auto-correlation of y');

figure(2)
plot(sxy,'k'); %cross-correlation
title('x-y cross-correlation');
axis([0 479 -150 150]);
