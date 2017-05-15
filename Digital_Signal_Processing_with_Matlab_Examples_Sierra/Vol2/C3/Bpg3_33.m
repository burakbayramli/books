% Convert Parrot picture to YIQ with I and Q coloured planes
parrot=imread('parrot1.jpg'); %read the image file into a matrix
parrotyiq=rgb2ntsc(parrot);

[M,N,P]=size(parrot);

figure(1)
subplot(1,3,1);
imshow(parrotyiq(:,:,1)); 
title('Y (intensity)');

subplot(1,3,2);
A=zeros(M,N,3);
A(:,:,1)=parrotyiq(:,:,1);
A(:,:,2)=parrotyiq(:,:,2);
Ii=ntsc2rgb(A);
imshow(Ii); 
title('I-color');

subplot(1,3,3);
A=zeros(M,N,3);
A(:,:,1)=parrotyiq(:,:,1);
A(:,:,3)=parrotyiq(:,:,3);
Iq=ntsc2rgb(A);
imshow(Iq); 
title('Q-color');

