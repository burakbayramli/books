% frequency response of IIR yulewalk filter
fs=130; %sampling frequency in Hz.
F=[0 0.3 0.5 0.7 1]; %frequency response specification (0 to 1)
M=[1 1 0 1 1]; %" "" ""
N=8; %order of the digital IIR filter
[numd,dend]=yulewalk(N,F,M); %filter computation

f=linspace(0,65); %linear set of frequency values in Hz.
G=freqz(numd,dend,f,fs); %computes frequency response
plot(F*fs/2,M,'--k'); hold on;
plot(f,abs(G),'k'); %plots gain
axis([0 60 0 1.2]);
ylabel('Gain'); title('Hf(w) 8th yulewalk filter')
xlabel('Hz.');

