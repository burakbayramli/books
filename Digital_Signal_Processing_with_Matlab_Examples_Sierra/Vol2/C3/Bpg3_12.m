% Fourier transform
% a simple edge
fg=[ones(256,128) zeros(256,128)]; %simple edge
Ffg=fftshift(fft2(fg)); %Fourier transform
M=max(max(Ffg)); % the one maximum value
sFfg=(256*Ffg/M); %normalization
figure(1)
subplot(1,2,1)
imshow(fg); %plots the binary image with a simple edge
title('Fourier transform of an edge');
ylabel('original');
subplot(1,2,2)
imshow(abs(sFfg)); %plots the Fourier transform
ylabel('transform');