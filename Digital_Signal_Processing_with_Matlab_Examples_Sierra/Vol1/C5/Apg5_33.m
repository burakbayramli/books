% frequency response of IIR invfreqz filter
fs=130; %sampling frequency in Hz.
F=[0 0.3 0.5 0.7 1]; %frequency response specification (0 to 1)
A=[1 1 0 1 1]; %amplitude
PH=[0 -0.5 -1 0.5 0]; %phase in rad

W=F*pi; %frequencies in rad/s
H=(A.*cos(PH))+(A.*sin(PH))*i; %complex frequency response

Nnum=2; Nden=4; %degree of the digital IIR filter numerator and denominator
[numd,dend]=invfreqz(H,W,Nnum,Nden); %filter computation

f=linspace(0,65); %linear set of frequency values in Hz.
G=freqz(numd,dend,f,fs); %computes frequency response
subplot(2,1,1)
plot(F*fs/2,A,'--k'); hold on;
plot(f,abs(G),'k'); %plots gain
ylabel('Gain'); title('Hf(w) invfreqz filter')
subplot(2,1,2)
plot(F*fs/2,PH,'--k'); hold on;
plot(f,angle(G),'k'); %plots gain
ylabel('Phase');
xlabel('Hz.');


