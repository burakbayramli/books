% Haar wavelet transform of a signal
% SCALOGRAM
% Sawtooth signal 
fy=600; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

fs=4*(10^5); %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
K=12; %exponent
Ns=2^K; %let us take 2^K signal samples
duy=Ns*tiv; %time for 2^K samples
t=0:tiv:(duy-tiv); %time intervals set

y=sawtooth(wy*t); %signal data set (256 samples)

wty=y; %space for the wavelet transform
d=zeros(K,Ns/2); %space for d(j,k) coefficients

NN=Ns;
%the Haar wavelet transform
for n=1:K,
   aux1= wty(1:2:NN-1) + wty(2:2:NN);
   aux2= wty(1:2:NN-1) - wty(2:2:NN);
   wty(1:NN)=[aux1,aux2]/sqrt(2);
   d(K+1-n,1:NN/2)=wty(1+(NN/2):NN); %fill d(j,k) coefficients
   NN=NN/2;
end;

%preparing for scalogram

S=zeros(K,Ns); %space for S(j,k) scalogram coefficients
for n=1:K,
   q=2^(n-1); L=Ns/q;
   for m=1:q,
    R=(1+(L*(m-1))):(L*m); %index range 
    S(n,R)=d(n,m);
   end;
end;
   
%figure
subplot('position',[0.04 0.77 0.92 0.18])
plot(y);
axis([0 4096 -1 1]);
title('signal');
subplot('position',[0.04 0.05 0.92 0.6])
imagesc(S); colormap('pink');
title('Scalogram of Haar w.t. sawtooth signal');
h=gca; set(h,'YDir','normal');
