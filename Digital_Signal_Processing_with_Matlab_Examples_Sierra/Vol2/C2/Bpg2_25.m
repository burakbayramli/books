% Visual Evoked Potential analysis
% Daubechies 4 Wavelet
% Plot of signal and scalogram

%The EVP signal 
fs=250; %samplig frequency in Hz
tiv=1/fs; %time interval between samples
Ts=tiv; %sampling period

%read signal file
fer=0;
while fer==0,  
fid2=fopen('EVP_short.txt','r');
if fid2==-1, disp('read error') 
else sg=fscanf(fid2,'%f \r\n'); fer=1;
end;
end;
fclose('all');

Nss=length(sg); %number of signal samples
duy=(Nss-1)*tiv; %duration of signal
tss=0:tiv:duy; %time intervals set

%analysis of the signal with wavelets
y=sg;

%scaling filter
hden=4*sqrt(2); %coeff. denominator
hsq=sqrt(3); %factor
h=[(1+hsq)/hden, (3+hsq)/hden, (3-hsq)/hden, (1-hsq)/hden]; %Daubechies 4
N=length(h);

K=9; %number of scales (512=2^9)
dd=zeros(K,Nss/2); %space for coefficients
a=y';
aux=0;
h0=fliplr(h);
h1=h; h1(1:2:N)=-h1(1:2:N);

%wavelet calculus using filters
NN=Nss;
for n=1:K,
   L=length(a); 
   a=[a(mod((-(N-1):-1),L)+1) a]; 
   d=conv(a,h1);
   d=d(N:2:(N+L-2));
   a=conv(a,h0);
   a=a(N:2:(N+L-2));
   aux=[d,aux];
   dd(K+1-n,1:NN/2)=d;
   NN=NN/2;
end;

wty=[a,aux(1:end-1)]; 

%preparing for scalogram

S=zeros(K,Nss); %space for S(j,k) scalogram coefficients
for n=1:K,
   q=2^(n-1); L=Nss/q;
   for m=1:q,
    R=(1+(L*(m-1))):(L*m); %index range 
    S(n,R)=dd(n,m);
   end;
end;
   
%figure
subplot('position',[0.04 0.77 0.92 0.18])
plot(y);
axis([0 512 1.2*min(y) 1.2*max(y)]);
title('signal');
subplot('position',[0.04 0.05 0.92 0.6])
imagesc(S); colormap('bone');
title('Scalogram of Daubechies w.t. Evoked Potential signal');
h=gca; set(h,'YDir','normal');

