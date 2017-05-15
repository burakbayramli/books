% Frequency domain Wiener filtering
% Image with motion blurring
clear all;
ux=imread('car1.tif');
[Ny,Nx]=size(ux);

%signal and noise between 0 and 1
x=im2double(ux);
vn=abs(0.1*randn(Ny,Nx)); v=mod(vn,1);

X=fftshift(fft2(x)); %Fourier transform of x

%motion description
T=1; %time in seconds
a=9; %vertical shift in pixels
b=5; %horizontal shift in pixels

%frequency domain motion blur filter (circular mask)
for nu=1:Ny,
   for nv=1:Nx,
      aux1=(pi*(a*nu+b*nv))/(Nx+Ny);
      BF(nu,nv)=(T*sin(aux1)*exp(-j*aux1))/aux1;
   end;
end;   
BF=2*pi*BF/T; %normalization

bx=abs(ifft2(X.*BF)); %blurred image
Kmix=0.85; %to be edited
bx=Kmix*bx; v=(1-Kmix)*v;
y=bx+v; %noise + blurred image

%Wiener computations
Sxx=abs(X).^2; %Sxx
V=fftshift(fft2(v)); %Fourier transform of v
Svv=abs(V).^2; %Svv
%Wiener filter
Wnum=conj(BF).*Sxx;
Wden1=(abs(BF).^2).*Sxx;
WH=Wnum./(Wden1+Svv); %Fourier transform of the Wiener filter

Y=fftshift(fft2(y)); %Fourier transform of y
fly=abs(ifft2(Y.*WH)); %apply the Wiener filter

%signals between 0 and 1
miy=min(min(y)); y=y-miy; %y>=0
may=max(max(y)); y=y/may; %y<=1
mify=min(min(fly)); fly=fly-mify; %fly>=0
mafy=max(max(fly)); fly=fly/mafy; %fly<=1

Uy=im2uint8(y); %convert to uint8 for display
Ufly=im2uint8(fly); %convert to uint8 for display

%display---------------------------
figure(1)
imshow(abs(BF)); %plots figure
title('motion blur mask');

figure(2)
imshow(Uy); %plots figure
title('Image with motion blur');

figure(3)
imshow(Ufly); %plots figure
title('deblurred image');
