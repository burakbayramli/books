%Wigner distribution of a chirp signal
clear all;
% chirp signal
f0=5; %initial frequency in Hz
f1=60; %final frequency in Hz
fs=128; %sampling rate in Hz
fN=fs/2; %Nyquist frequency
tiv=1/fs; %time between samples
t1=2; %final time
t=0:tiv:t1; %time intervals set (2 seconds)

yr=chirp(t,f0,t1,f1,'quadratic')'; %the chirp signal
y=hilbert(yr); %analitical signal

Ny=length(y); %odd number

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
    fo=fft(aux,Ny)/(Ny);
    WD(:,nt)=2*real(fo); %a column (harmonics at time nt)
end 

%result display
figure(1)
fiv=fN/Ny; %frequency interval
f=0:fiv:(fN-fiv); %frequency intervals set
colmap1; colormap(mapg1); %user colormap
imagesc(t,f,log10(0.5+abs(WD))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Wigner distribution of a chirp signal');
