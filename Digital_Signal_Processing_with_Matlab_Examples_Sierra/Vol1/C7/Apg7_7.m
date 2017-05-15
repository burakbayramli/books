% Wigner distribution of 2 GMPs signal
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

%result display
figure(1)
fiv=fN/Ny; %frequency interval
f=0:fiv:(fN-fiv); %frequency intervals set
colmap1; colormap(mapg1); %user colormap
imagesc(t,f,log10(0.01+abs(WD))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('Wigner distribution of the 2 GMPs signal');

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
