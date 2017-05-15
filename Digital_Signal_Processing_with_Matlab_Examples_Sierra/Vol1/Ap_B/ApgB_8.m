%Wigner distribution of a 2-sine signal
clear all
% 2-sine signal
f1=10; %initial frequency in Hz
f2=50; %final frequency in Hz
fs=128; %sampling rate in Hz
fN=fs/2; %Nyquist frequency
tiv=1/fs; %time between samples
t1=0:tiv:(4-tiv); %time of first signal part (4 seconds)
tn=4:tiv:5; %time inter-signal parts (1 seconds)
t2=5:tiv:(8-tiv); %time of last signal part (3 seconds)
y1=exp(-j*2*pi*f1*t1); y2=exp(-j*2*pi*f2*t2);
yn=0*exp(-j*2*pi*tn);
y=[y1 yn y2]'; %complete signal (column vector)
t=[t1 tn t2]; %complete signal time set
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
imagesc(t,f,log10(0.1+abs(WD))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Wigner distribution of a two-sine signal');

%Marginals-----------------------------------------
margf=zeros(Ny,1); %frequency marginal
for nn=1:Ny,
   margf(nn)=tiv*sum(WD(nn,:));
end;

margt=zeros(1,Ny); %time marginal
for nn=1:Ny,
   margt(nn)=sum(WD(:,nn)); 
end;

figure(2)
plot(f,margf,'k'); %frequency marginal
xlabel('Hz');
title('frequency marginal');

figure(3)
plot(t,margt,'k'); %time marginal
xlabel('seconds');
title('time marginal');

%print y signal energy
disp('signal energy:')
e1=tiv*sum(abs(margt))
e2=sum(abs(margf))
