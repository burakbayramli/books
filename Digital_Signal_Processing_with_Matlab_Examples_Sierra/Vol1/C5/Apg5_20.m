% h(n) and Hf(w) of (Remez) Parks-McClellan filter
fs=130; %sampling frequency in Hz.
fc=10/(fs/2); %cut-off at 10 Hz.
N=50; %even order
F=[0 fc fc+0.05 1]; %low-pass filter piecewise description
A=[1 1 0 0]; % " " "
numd=remez(N,F,A); %transfer function numerator
dend=[1]; %transfer function denominator

subplot(1,2,1)
stem(numd,'k'); %plots h(n)
axis([1 51 -0.05 0.2]); 
title('h(n)'); xlabel('n');

subplot(1,2,2) 
f=logspace(0,2,200); %logaritmic set of frequency values in Hz.
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'k'); %plots gain
axis([1 100 0 1.1]); grid;
xlabel('Hz.'); title('50th Parks-McClelland filter   Hf(w)')
