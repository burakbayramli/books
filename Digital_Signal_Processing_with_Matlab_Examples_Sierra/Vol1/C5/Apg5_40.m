% frequency response of IIR maxflat filter
fs=130; %sampling frequency in Hz.
fc=10; %cut-off at 10 Hz

wc=2*fc/fs; %normalized cut-off frequency (0 to 1)

Nnum=2; Nden=4; %degree of the digital IIR filter numerator and denominator
[numd,dend]=maxflat(Nnum,Nden,wc); %filter computation

subplot(1,2,1)
f=logspace(0,2); %logaritmic set of frequency values in Hz.
G=freqz(numd,dend,f,fs); %computes frequency response
semilogx(f,abs(G),'k'); %plots gain
axis([1 100 0 1.1]);
ylabel('Gain'); title('Hf(w) maxflat filter')
xlabel('Hz.'); grid;

subplot(1,2,2)
theta=0:.1:2*pi;
plot(cos(theta),sin(theta),'--k'); hold on; %draw a circunference
fdt=tf(numd,dend);
pzmap(fdt); hold on; %pole-zero map

