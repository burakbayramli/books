% PSF example (atmospheric turbulence)

%load the image
ip=imread('clock1.jpg');
ip=ip(:,:,1); %select one plane
op=im2double(ip); %convert to float

%create the PSF
[oL,oC]=size(op);
L=oL-1; C=oC-1; %number-1 of rows and columns
sigma=3;
[x,y]=meshgrid(-C/2:C/2, -L/2:L/2);
argm=-((x.^2)+(y.^2))/(2*sigma*sigma);

d=exp(argm); 
md=sum(d(:));
d=d/md; %the normalized PSF

%image blurring caused by PSF
OP=fft2(op);
D=fft2(d);
BP=D.*OP; %using Fourier for convolution
bp=abs(ifftshift(ifft2(BP)));

%display-------------------
figure(1)
mesh(x,y,d);
title('Gaussian PSF');

figure(2)
subplot(1,2,1)
imshow(op); title('original image')
subplot(1,2,2)
imshow(bp); title('blurred image')
