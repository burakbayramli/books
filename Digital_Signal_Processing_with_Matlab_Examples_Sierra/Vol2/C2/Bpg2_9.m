% Haar wavelet transform of a signal using filters
% Recovering of sawtooth signal from its transform

% The Haar filters, analysis part
c=1/sqrt(2);
h0=[c c]; %low-pass filter
h1=[-c c]; %high-pass filter

%-------------------------------------------------------
% First the wavelet transform to get the data

%The sawtooth signal
fy=300; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s
duy=0.03; %signal duration in seconds

fs=20000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
Ns=256; %let us take 256 signal samples
duy=Ns*tiv; %time for 256 samples
t=0:tiv:(duy-tiv); %time intervals set

y=sawtooth(wy*t); %signal data set (256 samples)

%Haar wavelet transform using filters
K=8; %exponent, 256=2^8
wty=y; %space for the wavelet transform
d=zeros(K,Ns/2); %space for d(j,k) coefficients
a=zeros(K,Ns/2); %space for a(j,k) coefficients
My=y; %auxiliar vector
Ls=zeros(1,128); %"  "  "
Hs=zeros(1,128); %"  "  "

for nn=K:-1:1, 
  m=2^nn;
  lx=filter(h0,1,My); Ls(1:(m/2))=lx(2:2:m); a(nn,1:(m/2))=Ls(1:(m/2)); %LP and subsampling
  hx=filter(h1,1,My); Hs(1:(m/2))=hx(2:2:m); d(nn,1:(m/2))=Hs(1:(m/2)); %HP and subsampling
  My=Ls(1:(m/2));
  wty((1+(m/2)):m)=d(nn,1:(m/2)); %append to wty
end;
wty(1)=a(1,1);

%-------------------------------------------------------
% Second the recovery from the wavelet transform
% The Haar filters, synthesis part
c=1/sqrt(2);
f0=[c c]; %low-pass filter
f1=[c -c]; %high-pass filter

J=K+1;
z=wty; %space for recovered data
a=zeros(J,(2^K)); %space for a(j,k) coefficients
Ma=zeros(1,(2^K)); Md=Ma; %auxiliar vectors

m=1;
a(1,1)=z(1); d(1,1)=z(2);
Ma(1)=a(1,1); Md(1)=d(1,1); 
for nn=1:(K-1),
   m=2^nn;
   lx=filter(f0,1,Ma); hx=filter(f1,1,Md); 
   a(nn+1,1:m)=lx(1:m)+hx(1:m);   
   Ma(1:2:(m*2))=a(nn+1,1:m); %upsampling
   Md(1:2:(m*2))=z((1+m):(m*2));  %" "  "  
end;
nn=8; m=2^nn;
lx=filter(f0,1,Ma); hx=filter(f1,1,Md); 
a(nn+1,1:m)=lx(1:m)+hx(1:m);

y=a(J,1:256); %the recovered data

%figure
plot(y,'k');
axis([0 256 -1.2 1.2]);
title('Recovered sawtooth from Haar wavelet transform using filters');
