% pole-zero map of IIR invfreqz filter
fs=130; %sampling frequency in Hz.
F=[0 0.3 0.5 0.7 1]; %frequency response specification (0 to 1)
A=[1 1 0 1 1]; %amplitude
PH=[0 -0.5 -1 0.5 0]; %phase in rad

W=F*pi; %frequencies in rad/s
H=(A.*cos(PH))+(A.*sin(PH))*i; %complex frequency response

Nnum=2; Nden=4; %degree of the digital IIR filter numerator and denominator
[numd,dend]=invfreqz(H,W,Nnum,Nden); %filter computation

theta=0:.1:2*pi; nn=length(theta);
plot(cos(theta),sin(theta),'--k'); hold on; %draw a circunference
fdt=tf(numd,dend);
pzmap(fdt); %pole-zero map


