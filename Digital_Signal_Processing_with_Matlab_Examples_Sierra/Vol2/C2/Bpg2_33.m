% Lifting example
% analysis of sonar signal
% Daubechies 4 Wavelet
% Plot of signal and scalogram

%The sonar signal 
[y1,fs1]=wavread('sonar1.wav'); %read wav file

ta=7815; tb=ta+255;
sg=y1(ta:tb);
Nss=length(sg);%number of signal samples
tiv=1/fs1;
duy=(Nss-1)*tiv; %duration of signal
tss=0:tiv:duy; %time intervals set
Ts=tiv; %sampling period

%analysis of the signal with wavelets
y=sg;

a=y';
aux=0; cq=sqrt(3);
K=8; %number of scales (256=2^8)
%wavelet calculus using lifting
NN=Nss;
for n=1:K,
   L=length(a); L2=L/2;
   a1=a(1:2:L-1)+ (cq*a(2:2:L));
   d1=a(2:2:L)-((cq/4)*a1)-(((cq-2)/4)*[a1(L2) a1(1:(L2-1))]);
   a2=a1-[d1(2:L2) d1(1)];
   a=((cq-1)/sqrt(2))*a2;
   d=((cq+1)/sqrt(2))*d1;
   aux=[d,aux];
   dd(K+1-n,1:NN/2)=d;
   NN=NN/2;
end;

wty=[a,aux(1:(end-1))];

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
axis([0 256 1.2*min(y) 1.2*max(y)]);
title('signal');
subplot('position',[0.04 0.05 0.92 0.6])
imagesc(S); colormap('bone');
title('Scalogram of Daubechies w.t. sonar fragment');
h=gca; set(h,'YDir','normal');

