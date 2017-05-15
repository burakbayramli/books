% Comparison of stop-band of Hw(w) of Hanning, Hamming and Blackman windows
fs=130; %sampling frequency in Hz.
N=50; %even order
f=logspace(0,2,400); %logaritmic set of frequency values in Hz.
dend=[1]; %transfer function denominator

hw=hanning(N+1); %Hanning window
numd=2*hw/N; %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
subplot(1,3,1);semilogx(f,abs(G),'k'); %plots gain
axis([1 100 0 0.04]); title('Hanning');
grid;

hw=hamming(N+1); %Hamming window
numd=2*hw/N; %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
subplot(1,3,2); semilogx(f,abs(G),'r'); %plots gain
axis([1 100 0 0.04]); title('Hamming');
grid;

hw=blackman(N+1); %Blackman window
numd=2*hw/N; %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
subplot(1,3,3); semilogx(f,abs(G),'b'); %plots gain
axis([1 100 0 0.04]); title('Blackman');
grid;

xlabel('Hz.');