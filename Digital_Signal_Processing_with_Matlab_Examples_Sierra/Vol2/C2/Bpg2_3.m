% Haar wavelet transform of a signal
% Sawtooth signal 
fy=300; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

fs=20000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
Ns=256; %let us take 256 signal samples
duy=Ns*tiv; %time for 256 samples
t=0:tiv:(duy-tiv); %time intervals set

y=sawtooth(wy*t); %signal data set (256 samples)

K=8; %exponent, 256=2^8
wty=y; %space for the wavelet transform
d=zeros(K,Ns/2); %space for d(j,k) coefficients

%the Haar wavelet transform
for n=1:K,
   aux1= wty(1:2:Ns-1) + wty(2:2:Ns);
   aux2= wty(1:2:Ns-1) - wty(2:2:Ns);
   wty(1:Ns)=[aux1,aux2]/sqrt(2);
   d(K+1-n,1:Ns/2)=wty(1+(Ns/2):Ns); %fill d(j,k) coefficients
   Ns=Ns/2;
end;

%figure
  %scaling
dmax=max(max(d));
dmin=min(min(d)); abdmin=abs(dmin);
if abdmin>dmax, mh=abdmin; else mh=dmax; end;
  %area and signal
  plot([0 270],[0 0],'b'); hold on;
  plot(y,'k'); %the signal
axis([0 270 -1.2 20]);
%subplots
for nn=1:8,
   nx=2^(nn-1);
   ylevel=20-(2.2*nn);
   plot([0 270],[ylevel ylevel],'b'); %horizontal axes
   ydat=d(nn,1:nx)/mh; %data scaling
   yno=zeros(1,nx); 
   ivx=256/nx; %horizontal interval
   for kk=1:nx,
      plot([ivx*(kk) ivx*(kk)],[ylevel+yno(kk) ylevel+ydat(kk)],'k');
      plot([ivx*(kk) ivx*(kk)],[ylevel+ydat(kk) ylevel+ydat(kk)],'rx');
   end;   
end;
title('Haar wavelet transform of sawtooth signal');