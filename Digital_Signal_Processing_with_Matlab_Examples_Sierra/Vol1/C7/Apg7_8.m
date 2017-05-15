% SAF of 2 GMPs signal
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

%result display
figure(1)
fiv=fN/Ny; %frequency interval
freq=-fN/2:fiv:(fN/2)-fiv; te=t(end); tim=-te/2:tiv:te/2;
colmap1; colormap(mapg1); %user colormap
imagesc(tim,freq,log10(0.005+abs(SAF))); axis xy;
xlabel('seconds'); ylabel('Hz'); 
title('SAF of the 2 GMPs signal');

%Energy and autocorrelations---------------------------------------
 
tcorr=zeros(1,Ny); %temporal autocorrelation
nt=1:Ny; %vector (for indexes)
for mtau=-md:md,
   aux=sum(zyz(Ny+nt).*conj(zyz(Ny+nt-(2*mtau))));    
   tcorr(md+mtau+1)=tiv*aux; 
end;

fcorr=zeros(Ny,1); %frequencial autocorrelation
zerf=zeros(Ny,1);
YW=fft(y,Ny)/Ny; ZYWZ=[zerf;YW;zerf];
nf=1:fs; %vector (for indexes)
mf=fs/2;
for mtheta=-mf:mf,
   aux=sum(ZYWZ(fs+nf).*conj(ZYWZ(fs+nf-mtheta)));
   fcorr(mf+mtheta+1)=(Ny/fs)*aux; 
end;

of=(Ny+1)/2; ot=(Ny+1)/2; %SAF origin

figure(2) %frequencial autocorrelation
plot(freq,abs(fcorr),'rx'); hold on;
plot(freq,abs(SAF(:,ot)),'k');
xlabel('Hz');
title('frequencial autocorrelation');

figure(3) %temporal autocorrelation
plot(tim,abs(tcorr),'rx'); hold on;
plot(tim,abs(SAF(of,:)),'k');
xlabel('seconds');
title('temporal autocorrelation');

%print y signal energy
disp('signal energy:')
o1=SAF(of,ot)

