% Fourier transform
% rhombus
fg=ones(256,256); %white plane
for n=1:64,
   fgx=128+(-n:n); fgy=64+n;
   fg(fgx,fgy)=0; %one triangle
   fg(fgx,256-fgy)=0; %the other triangle
end
Ffg=fftshift(fft2(fg)); %Fourier transform
M=max(max(Ffg)); % the one maximum value
sFfg=(256*Ffg/M); %normalization
figure(1)
subplot(1,2,1)
imshow(fg); %plots the binary image
title('Fourier transform of a rhombus');
ylabel('original');
subplot(1,2,2)
imshow(abs(sFfg)); %plots the Fourier transform
ylabel('transform');

