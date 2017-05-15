% Impulse response of raised cosine filter
fs=130; %sampling frequency in Hz.
fc=10; %cut-off at 10 Hz.
beta=0; %roll-off factor
N=50; %even order
numd=firrcos(N,fc,beta,fs,'rolloff'); %transfer function numerator
dend=[1]; %transfer function denominator
plot(numd,'-xk'); hold on; %plots impulse response
axis([1 51 -0.05 0.18]);
title('Impulse response of 50th raised cosine filter');

for beta=0.25:0.25:1,
numd=firrcos(N,fc,beta,fs,'rolloff'); %transfer function numerator
plot(numd,'-xk'); %plots impulse response
end   