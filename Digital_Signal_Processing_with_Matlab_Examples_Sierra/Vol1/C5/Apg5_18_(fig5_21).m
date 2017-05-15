% frequency response Hf(w) of a filter
fs=130; %sampling frequency in Hz.
fc=10/(fs/2); %cut-off at 10 Hz.
N=50; %even order
hw=boxcar(N+1);
numd=fir1(N,fc,hw); %transfer function numerator
dend=[1]; %transfer function denominator

f=linspace(0.1,20); %
G=freqz(numd,dend,f,fs); %computes frequency response
plot(f,abs(G),'k'); %plots gain
axis([0.1 20 0 1.4]);
ylabel('Gain'); xlabel('Hz.'); title('Frequency response of a filter')
