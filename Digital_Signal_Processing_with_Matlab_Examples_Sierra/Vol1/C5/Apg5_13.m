% Comparison of  Hw(w) of Hanning, Hamming and Blackman windows
fs=130; %sampling frequency in Hz.
N=50; %even order
f=logspace(0,2,200); %logaritmic set of frequency values in Hz.
dend=[1]; %transfer function denominator

hw=hanning(N+1); %Hanning window
numd=2*hw/N; %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'k'); %plots gain
hold on;

hw=hamming(N+1); %Hamming window
numd=2*hw/N; %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'r'); %plots gain

hw=blackman(N+1); %Blackman window
numd=2*hw/N; %transfer function numerator
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'b'); %plots gain

axis([1 100 0 1]); title('Hw(w) of Hanning, Hamming and Blackman windows');
grid; xlabel('Hz.');
