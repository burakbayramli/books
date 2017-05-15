% SAF from Wigner distribution of 2 GMPs signal
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
Ny=length(t); %odd number
yn=zeros(1,Ny-(2*Np)); %intermediate signal
yr=[y1 yn y2]'; %2 GMPs signal (column vector)
y=hilbert(yr); %analytical signal

%WIGNER-------------------------------------------
zerx=zeros(Ny,1); aux=zerx;
lm=(Ny-1)/2;
zyz=[zerx; y; zerx]; %sandwich zeros-signal-zeros
WD=zeros(Ny,Ny); %space for the Wigner distribution, a matrix
mtau=0:lm; %vector(used for indexes)
for nt=1:Ny, 
    tpos=Ny+nt+mtau; %a vector
    tneg=Ny+nt-mtau; %a vector
    aux(1:lm+1)=(zyz(tpos).*conj(zyz(tneg)));
    aux(1)=0.5*aux(1); %will be added 2 times
    fo=fft(aux,Ny)/Ny;
    WD(:,nt)=2*real(fo); %a column (harmonics at time nt)
end 

pks=WD; %intermediate variable
ax1=ifft(pks,[],2);
SAF=fftshift(fft(ax1,[],1)'); %SAF from Wigner distribution

%result display
fiv=fN/Ny; %frequency interval
freq=-fN/2:fiv:(fN/2)-fiv; te=t(end); tim=-te/2:tiv:te/2;
colmap1; colormap(mapg1); %user colormap
imagesc(tim,freq,log10(0.005+abs(SAF))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('SAF of the 2 GMPs signal (from Wigner)');
