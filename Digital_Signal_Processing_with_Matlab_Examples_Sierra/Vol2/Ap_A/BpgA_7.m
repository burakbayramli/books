% Example of general fan filter
% filtering on 2-D Fourier domain
%
phot=imread('Ben1.tif'); %read the 256x256 image file into a matrix

N=256;
L=N/2;

%filter specification
alpha=1; beta=0.1; %prototype
theta=pi/3; 
a=1/tan(theta/2); %aperture
phi=0.3*pi; %direction

cp=cos(phi); sp=sin(phi);
aux1=[-1 -3 -1; 0 0 0; 1 3 1];
aux2=[-1 0 1; -3 0 3; -1 0 1];
P=(cp*aux1)-(sp*aux2);
Q=(sp*aux1)+(cp*aux2);

B=alpha*Q;
aux3=(a*P)+(beta*Q);
A=B+(j*aux3);

H=zeros(N,N);

%frequency response of the fan filter
% (first quarter) 
AX=zeros(1,L);
for ny=1:L,
   nl=L+1-ny;   
   w2=(ny*pi)/N; z2=exp(j*w2);
   for nx=1:L,
      nc=L+nx;
      w1=(nx*pi)/N; z1=exp(j*w1);
      VZ1=[1 z1 z1^2]; VZ2=[1 z2 z2^2];
      hnum=VZ1*B*VZ2'; hden=VZ1*A*VZ2';
      if abs(hden)<1.0e-3, hnum=100; hden=1; end;
      H(nl,nc)=hnum/hden;
   end;
end;  
BX=zeros(1,L);
% (second quarter, counterclockwise)
for ny=1:L,
   nl=L+1-ny;
   w2=(ny*pi)/N; z2=exp(j*w2);
   for nx=1:L,
      nc=L+1-nx;
      w1=-(nx*pi)/N; z1=exp(j*w1);
      VZ1=[1 z1 z1^2]; VZ2=[1 z2 z2^2];
      hnum=VZ1*B*VZ2'; hden=VZ1*A*VZ2';
      if abs(hden)<1.0e-3, hnum=100; hden=1; end;
      H(nl,nc)=hnum/hden;        
   end;
end; 
% (third quarter)
for ny=1:L,
   nl=L+ny;
   w2=-(ny*pi)/N; z2=exp(j*w2);
   for nx=1:L,
      nc=L+1-nx;
      w1=-(nx*pi)/N; z1=exp(j*w1);
      VZ1=[1 z1 z1^2]; VZ2=[1 z2 z2^2];
      hnum=VZ1*B*VZ2'; hden=VZ1*A*VZ2';
      if abs(hden)<1.0e-3, hnum=100; hden=1; end;
      H(nl,nc)=hnum/hden;
   end;
end; 
% (fourth quarter)
for ny=1:L,
   nl=L+ny;
   w2=-(ny*pi)/N; z2=exp(j*w2);
   for nx=1:L,
      nc=L+nx;
      w1=(nx*pi)/N; z1=exp(j*w1);
      VZ1=[1 z1 z1^2]; VZ2=[1 z2 z2^2];
      hnum=VZ1*B*VZ2'; hden=VZ1*A*VZ2';
      if abs(hden)<1.0e-3, hnum=100; hden=1; end;
      H(nl,nc)=hnum/hden;
   end;
end; 

%H=abs(H);
%filtering (Fourier domain)
Fph=fftshift(fft2(phot));
Fphfil=Fph.*H;
Iph=ifft2(Fphfil);
uIp=uint8(abs(Iph));

%display
figure(1)
uH=uint8(256-abs(256*H)); %invert color
imshow(uH);
title('Support of the general fan filter');

figure(2)
imshow(uIp);
title('Filtered image');
