% WD of 2 GMPs signal, with no interference
clear all
% 2 GMPs signal
fy1=40; %signal 1 central frequency in Hz
fy2=80; %signal 2 central frequency
bw=0.2; %signal relative bandwidth
fs=300; %sampling frequency in Hz
fN=fs/2; %Nyquist frequency
tiv=1/fs; %time interval between samples;
tp=-(0.2-tiv):tiv:(0.2-tiv); %time intervals set (0.4 seconds)
Np=length(tp); 
y1=gauspuls(tp,fy1,bw); %signal 1 data set
y2=gauspuls(tp,fy2,bw); %signal 2 data set
t=0:tiv:1; %complete time set (1 second);
Ny=length(t); % odd number
yn=zeros(1,Ny-(2*Np)); %intermediate signal
yr=[y1 yn y2]'; %2 GMPs signal (column vector)
y=hilbert(yr); %analytical signal

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
HV=50; HH=70; %window vertical and horizontal 1/2 width
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
imagesc(tim,freq,log10(0.005+abs(FI))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Filter window (kernel)');

subplot(2,1,2)
imagesc(tim,freq,log10(0.005+abs(fsaf))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Filtered SAF');

%result display
figure(2)
fiv=fN/Ny; %frequency interval
f=0:fiv:(fN-fiv); %frequency intervals set
colmap1; colormap(mapg1); %user colormap
imagesc(t,f,log10(0.01+abs(WD))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Filtered Wigner distribution of the 2 GMPs signal');

