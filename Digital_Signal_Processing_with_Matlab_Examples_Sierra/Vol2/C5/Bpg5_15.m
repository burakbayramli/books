% Frequency domain Wiener filtering
% Blurred image with noise
clear all;
ux=imread('antena1.tif');
[Ny,Nx]=size(ux);

%signal and noise between 0 and 1
x=im2double(ux);
vn=abs(0.2*randn(Ny,Nx)); v=mod(vn,1);

X=fftshift(fft2(x)); %Fourier transform of x

%frequency domain blurring filter (circular mask)
dx=(0:(Nx-1))-round(Nx/2); dy=(0:(Ny-1))-round(Ny/2);
%a grid with the same size as image, with center (0,0):
[gx,gy]=meshgrid(dx,dy); 
R=sqrt((gx.^2)+(gy.^2));
BF=0.2+(R<15); %circle (low-pass filter)

bx=abs(ifft2(X.*BF)); %blurred image
Kmix=0.75; %to be edited
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

%signal between 0 and 1
mify=min(min(fly)); fly=fly-mify; %fly>=0
mafy=max(max(fly)); fly=fly/mafy; %fly<=1

Ufly=im2uint8(fly); %convert to uint8 for display

%display---------------------------

figure(1)
imshow(Ufly); %plots figure
title('deblurred with Wiener filter');
