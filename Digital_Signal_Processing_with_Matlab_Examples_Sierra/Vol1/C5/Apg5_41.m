% comparing filter with filtfilt
fs=130; %sampling frequency in Hz.
fc=10; %cut-off at 10 Hz

wc=2*fc/fs; %normalized cut-off frquency (0 to 1)

% a chebyshev1 IIR filter
N=6; %order of the filter
R=0.5; %ripple in the passband
[numd,dend]=cheby1(N,R,wc); %filter computation

%sawtooth input signal
fu=8; %signal frequency in Hz
wu=2*pi*fu; %signal frequency in rad/s
tiv=1/fs; %time intervals between samples
t=0:tiv:(0.5-tiv); %time intervals set (0.5 seconds)
u=sawtooth(wu*t); %sawtooth signal

subplot(2,1,1)
y=filter(numd,dend,u); %filter output
plot(t,u,'r'); hold on
plot(t,y,'k');
title('cheby1() and filter() result');
xlabel('seconds')

subplot(2,1,2)
z=filtfilt(numd,dend,u); %filtfilt output
plot(t,u,'r'); hold on
plot(t,z,'k');
title('cheby1() and filtfilt() result');
xlabel('seconds')


