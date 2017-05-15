%Wigner distribution of a warped modulated signal

fs=50; %sampling frequency in Hz
tiv=1/fs; %time between samples
t=tiv:tiv:(10+tiv); %time intervals set (10 seconds)(t>0)

fsig=5; %signal base frequency in Hz
wsig=fsig*2*pi; %signal base frequency in rad/s
K=1.4; %modulation exponent
oy=exp(-i*wsig*(t.^K))'; %the original modulated signal
Ny=length(oy); %odd number
fiv=fs/(2*Ny);
f=fiv:fiv:(fs/2); %frequencies set

Cex=(1-K)/(2*K);
Cwp=(t.^Cex)/sqrt(K); %factor
y=(Cwp.*exp(-i*wsig*t))'; %warped signal

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

wrt=zeros(Ny,1); fcc=zeros(Ny,1);
wrt=t.^K; %warped time
fcc=(t.^(1-K))/K; %frequency conversion coefficient 

%result display
figure(1)
colmap1; colormap(mapg1); %user colormap
imagesc(wrt,f,log10(0.1+abs(WD))); axis xy;
title('Wigner distribution of the warped modulated signal');
ylabel('base frequency'); xlabel('warped time');

figure(2)
plot(t,wrt,'k');
title('time warping'); grid;
xlabel('t'); ylabel('warped time');

figure(3)
plot(t,fcc,'k');
title('frequency conversion along time'); grid;
xlabel('t'), ylabel('fcc');

