% Impulse train autocovariance
fs=200; %samplig frequency in Hz
tiv=1/fs; %time interval between samples
t=0:tiv:(25-tiv); %time intervals set (25 seconds)
Ns=25*fs; %number of signal samples (25 seconds)

yp1=[1,zeros(1,199)];
yp=yp1;
for nn=1:24,
   yp=cat(2,yp,yp1);
end

subplot(2,1,1) %signal plot
plot(t,yp,'k')
axis([0 25 -0.2 1.2]);
title('Periodic impulse train');

subplot(2,1,2) %autocovariance plot
av=xcov(yp); %signal autocovariance
plot(t(1:Ns),av(Ns:((2*Ns)-1)),'k');  %plots autocovariance
xlabel('seconds'); title('autocovariance');
