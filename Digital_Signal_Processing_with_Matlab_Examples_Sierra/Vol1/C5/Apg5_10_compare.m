% Frequency response Hf(w) of Hamming windowed filter
% for several filter orders
fs=130; %sampling frequency in Hz.
fc=10/(fs/2); %cut-off at 10 Hz.
f=logspace(0,2); %logaritmic set of frequency values in Hz.
dend=[1]; %transfer function denominator

for nn=1:6,  
N=10*nn; %filter order
hw=hamming(N+1);
numd=fir1(N,fc,hw); %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
subplot(2,3,nn); semilogx(f,abs(G),'k'); %plots gain
axis([1 100 0 1.1]); grid;
end

xlabel('Hz.'); title('Hf(w) 50th Hamming windowed filter')
