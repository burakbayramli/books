% effect of a differentiator
fs=130; %sampling frequency in Hz.
N=50; %even order
F=[0.01 1]; %specification of frequency band
A=[0.01 1]; %specification of amplitudes
numd=remez(N,F,A,'differentiator'); %transfer function numerator
dend=[1]; %transfer function denominator

fu=5; %signal frequency in Hz
wu=2*pi*fu; %signal frequency in rad/s
tiv=1/fs; %time intervals between samples
t=0:tiv:(1-tiv); %time intervals set (1 seconds)
u=square(wu*t); %signal data set
y=filter(numd,dend,u); %filter response
plot(t,u,'-xr'); hold on;
plot(t,real(y),'k')
axis([0 1 -1.2 1.2]);
title('response of a differentiator');
xlabel('seconds');

