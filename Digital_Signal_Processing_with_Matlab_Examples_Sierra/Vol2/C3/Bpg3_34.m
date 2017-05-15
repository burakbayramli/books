% Modifiy YIQ Parrot picture
parrot=imread('parrot1.jpg'); %read the image file into a matrix
parrotyiq=rgb2ntsc(parrot);

newparrot(:,:,1)=0.8*(parrotyiq(:,:,1)); %change of Y
newparrot(:,:,2)=1.5*(parrotyiq(:,:,2)); %change of I
newparrot(:,:,3)=0.3*(parrotyiq(:,:,3)); %change of Q

figure(1)
subplot(1,2,1)
imshow(parrot); %original
title('original');

subplot(1,2,2)
newparrotrgb=ntsc2rgb(newparrot); %to RGB
imshow(newparrotrgb);
title('modified');


