% Inverse filter
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

%Inverse filtering
Y=fftshift(fft2(y)); %Fourier transform of y
fly=abs(ifft2(Y./BF)); %apply the inverse filter

%signals between 0 and 1
miy=min(min(y)); y=y-miy; %y>=0
may=max(max(y)); y=y/may; %y<=1
mify=min(min(fly)); fly=fly-mify; %fly>=0
mafy=max(max(fly)); fly=fly/mafy; %fly<=1

Uy=im2uint8(y); %convert to uint8 for display
Ufly=im2uint8(fly); %convert to uint8 for display

%display---------------------------
figure(1)
imshow(BF); %plots figure
title('blurring mask');

figure(2)
imshow(Uy); %plots figure
title('blurred image with noise');

figure(3)
imshow(Ufly); %plots figure
title('deblurred with inverse filter');
