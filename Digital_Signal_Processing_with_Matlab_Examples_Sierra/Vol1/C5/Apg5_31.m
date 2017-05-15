% pole-zero map of IIR yulewalk filter
fs=130; %sampling frequency in Hz.
F=[0 0.3 0.5 0.7 1]; %frequency response specification (0 to 1)
M=[1 1 0 1 1]; %" "" ""
N=8; %order of the digital IIR filter
[numd,dend]=yulewalk(N,F,M); %filter computation

theta=0:.1:2*pi; nn=length(theta);
plot(cos(theta),sin(theta),'--k'); hold on; %draw a circunference
fdt=tf(numd,dend);
pzmap(fdt); %pole-zero map

