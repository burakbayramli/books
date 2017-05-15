%Wigner distribution of a modulated signal

fs=50; %sampling frequency in Hz
tiv=1/fs; %time between samples
t=tiv:tiv:(10+tiv); %time intervals set (10 seconds)(t>0)

fsig=5; %signal base frequency in Hz
wsig=fsig*2*pi; %signal base frequency in rad/s
K=1.4; %modulation exponent
y=exp(-i*wsig*(t.^K))'; %the modulated signal
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
fiv=fs/(2*Ny);
f=fiv:fiv:(fs/2); %frequencies set

figure(1)
colmap1; colormap(mapg1); %user colormap
imagesc(t,f,log10(0.1+abs(WD))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Wigner distribution of a modulated signal');

