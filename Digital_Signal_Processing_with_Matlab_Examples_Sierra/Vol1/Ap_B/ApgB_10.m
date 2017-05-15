% WD of chirp signal, with no interference
clear all
% chirp signal
f0=5; %initial frequency in Hz
f1=60; %final frequency in Hz
fs=128; %sampling rate in Hz
fN=fs/2; %Nyquist frequency
tiv=1/fs; %time between samples
t1=2; %final time
t=0:tiv:t1; %time intervals set (10 seconds)

yr=chirp(t,f0,t1,f1,'quadratic')'; %the chirp signal
y=hilbert(yr); %analitical signal

Ny=length(y); %odd number

%SAF------------------------------------------------------
zerx=zeros(Ny,1); %a vector
zyz=[zerx; y; zerx]; %sandwich zeros-signal-zeros
aux=zerx;
SAF=zeros(Ny, Ny); %space for the SAF, a matrix
nt=1:Ny; %vector (used for indexes)
md=(Ny-1)/2;
for mtau=-md:md, 
    tpos=Ny+nt+mtau; %a vector
    tneg=Ny+nt-mtau; %a vector
    aux=zyz(tpos).*conj(zyz(tneg));
    SAF(:,md+mtau+1)=fftshift(fft(aux,Ny)/Ny); %a column (frequencies)
end 
 
 %A simple box distribution kernel
FI=zeros(Ny,Ny);
HV=30; HH=40; %window vertical and horizontal 1/2 width
FI(md-HH:md+HH,md-HV:md+HV)=1; %box kernel

%Product of kernel and SAF
fsaf=FI.*SAF;

pks=ifftshift(fsaf); %intermediate variable
ax=((ifft(pks,[],1)));
WD=real((fft(ax,[],2))'); %Wigner from SAF distribution

%result display
figure(1)
fiv=fN/Ny; %frequency interval
freq=-fN/2:fiv:(fN/2)-fiv; te=t(end); tim=-te/2:tiv:te/2;
colmap1; colormap(mapg1); %user colormap
subplot(2,1,1)
imagesc(tim,freq,log10(0.05+abs(FI))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Filter window (kernel)');

subplot(2,1,2)
imagesc(tim,freq,log10(0.1+abs(fsaf))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Filtered SAF');

%result display
figure(2)
fiv=fN/Ny; %frequency interval
f=0:fiv:(fN-fiv); %frequency intervals set
colmap1; colormap(mapg1); %user colormap
imagesc(t,f,log10(1+abs(WD))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Filtered Wigner distribution of the 2 GMPs signal');

