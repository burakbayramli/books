% Filtering the Fourier transform and then inverse
% high-pass Butterworth filter

keaton=imread('keaton2bw.tif'); %read the image file into a matrix
[ly,lx]=size(keaton); %determine image size
dx=(0:(lx-1))-round(lx/2); dy=(0:(ly-1))-round(ly/2);
[x,y]=meshgrid(dx,dy); %a grid with the same size as image, with center (0,0)
r=sqrt((x.^2)+(y.^2));
M=8; nf=3; %filter specification
fbut=1./(1+((r/M).^(2*nf))); %Butterworth filter
fhpbut=1-fbut; %high-pass filter
FKe=fftshift(fft2(keaton)); %Fourier transform
FKefil=FKe.*fhpbut; %filtering in the frequency domain
IKe=ifft2(FKefil); %inverse transform
uIKe=uint8(abs(IKe)); %convert to unsigned 8-bit

figure(1)
imshow(fhpbut); %the filter
title('Fourier Butterworth high-pass filtering');
ylabel('the filter');

figure(2)
mesh(x,y,fhpbut); %3D view of the filter
title('3D view of the filter');

figure(3)
imshow(uIKe); %display the photo
title('filtered image');


