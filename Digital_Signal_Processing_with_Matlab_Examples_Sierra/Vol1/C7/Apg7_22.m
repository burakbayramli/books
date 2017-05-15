%Unwarping the Wigner distribution of the warped signal
clear all
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

%Unwarping----------------

UWD=zeros(Ny,Ny);
for j=1:Ny, %times
   kk=1; k=1;
   while k<=Ny, %frequencies  
      kk=1+round(k/fcc(j));
      if kk<=Ny, 
         UWD(kk,j)=WD(k,j); %expansion
      else   
         k=Ny;
      end;
      k=k+1;
   end 
end 
   
%result display
figure(1)
colmap1; colormap(mapg1); %user colormap
imagesc(t,f,log10(0.1+abs(UWD))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Unwarped Wigner distribution');
