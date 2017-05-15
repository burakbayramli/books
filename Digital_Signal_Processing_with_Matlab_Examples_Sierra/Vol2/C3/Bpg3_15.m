% Fourier transform and inverse
keaton=imread('keaton1bw.tif'); %read the image file into a matrix
FKe=fftshift(fft2(keaton)); %Fourier transform
M=max(max(FKe)); % the one maximum value
sFKe=(256*FKe/M); %normalization
IKe=ifft2(FKe); %inverse transform
uIKe=uint8(abs(IKe)); %convert to unsigned 8-bit

figure(1)
subplot(1,3,1)
imshow(keaton); %display the photo
title('Fourier transform');
ylabel('original');
subplot(1,3,2)
imshow(abs(sFKe)); %Fourier transform of the photo
ylabel('transform');
subplot(1,3,3)
imshow(uIKe); %inverse of the Fourier transform
ylabel('inverse');