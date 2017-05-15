% Signal analysis by continuous wavelet transform
% Morlet Wavelet
% Plot of signal and scalogram

% Square signal 
fy=1; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s
duy=3; %signal duration in seconds

fs=20; %sampling frequency in Hz
Ts=1/fs; %time interval between samples;
t=0:Ts:(duy-Ts); %time intervals set

y=square(wy*t); %signal data set

ND=length(y); %number of data

CC=zeros(40,ND);
% CWT
nn=1:ND;
for ee=1:200,
   s=ee*0.008; %scales
   for rr=1:ND, %delays
      a=Ts*(rr-1);
      val=0;     
      %vectorized part
         t=Ts*(nn-1);
         x=(t-a)/s; %plug coeffs.
         psi=(1/sqrt(s))*(exp(-(x.^2)/2).*cos(5*x)); %wavelet
         for j=1:ND,
            val=val+(y(j).*psi(j));
         end;   
      CC(ee,rr)=val;
   end;
 end;


figure (1)
subplot(2,1,1)
plot(t,y,'k');
axis([0 duy min(y)-0.1 max(y)+0.1]);
xlabel('sec'); ylabel('signal');
title('wavelet analysis');
subplot(2,1,2)
imagesc(CC);
colormap('jet'); 
xlabel('samples'); ylabel('scales');

