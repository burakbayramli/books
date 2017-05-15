% Fourier transform
% a square
fg=ones(256,256); %white plane
fg(64:192, 64:192)=0; %insert black square
Ffg=fftshift(fft2(fg)); %Fourier transform
M=max(max(Ffg)); % the one maximum value
sFfg=(256*Ffg/M); %normalization
figure(1)
subplot(1,2,1)
imshow(fg); %plots the binary image 
title('Fourier transform of a square');
ylabel('original');
subplot(1,2,2)
imshow(abs(sFfg)); %plots the Fourier transform
ylabel('transform');

