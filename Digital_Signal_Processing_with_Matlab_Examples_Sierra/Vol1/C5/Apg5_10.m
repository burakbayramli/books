% hw(n) of Hamming window, and frequency response Hf(w) of windowed filter
fs=130; %sampling frequency in Hz.
fc=10/(fs/2); %cut-off at 10 Hz.
N=50; %even order
hw=hamming(N+1);
numd=fir1(N,fc,hw); %transfer function numerator
dend=[1]; %transfer function denominator

subplot(1,2,1)
stem(hw,'k'); %plots hw(n)
axis([1 51 0 1.2]); 
title('Hamming hw(n)'); xlabel('n');

subplot(1,2,2) 
f=logspace(0,2); %logaritmic set of frequency values in Hz.
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'k'); %plots gain
axis([1 100 0 1.1]); grid;
ylabel('Gain'); xlabel('Hz.'); title('Hf(w) 50th windowed filter')
