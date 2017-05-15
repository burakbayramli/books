% Impulse response of the rectangular filter

%the rectangular filter in 2-D Fourier domain
a=20; b=10; %rectangle half sides
Ffilt=zeros(256,256);
Ffilt((129-b):(128+b),(129-a):(128+a))=1;

%inverse transform
hfilt=ifftshift(ifft2(Ffilt));
ah=abs(hfilt);

figure(1)
imagesc(ah);
title('Impulse response');

