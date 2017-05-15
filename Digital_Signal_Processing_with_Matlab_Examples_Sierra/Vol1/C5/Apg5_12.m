% Comparison of Hf(w) of Hanning, Hamming and Blackman windowed filters
fs=130; %sampling frequency in Hz.
fc=10/(fs/2); %cut-off at 10 Hz.
N=50; %even order
f=logspace(0,2); %logaritmic set of frequency values in Hz.
dend=[1]; %transfer function denominator

hw=hanning(N+1); %Hanning window
numd=fir1(N,fc,hw); %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'k'); %plots gain
hold on;

hw=hamming(N+1); %Hamming window
numd=fir1(N,fc,hw); %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'r'); %plots gain

hw=blackman(N+1); %Blackman window
numd=fir1(N,fc,hw); %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'b'); %plots gain

axis([1 100 0 1.1]);
grid;
ylabel('Gain'); xlabel('Hz.'); title('Hf(w) of 50 th Hanning, Hamming and Blackman windowed filter')
