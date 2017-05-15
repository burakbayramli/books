% Display Coloured RGB planes of Parrot picture
parrot=imread('parrot1.jpg'); %read the image file into a matrix

[M,N,P]=size(parrot);

figure(1)
subplot(1,3,1);
A=uint8(zeros(M,N,3));
A(:,:,1)=parrot(:,:,1);
imshow(A); %display R

subplot(1,3,2);
A=uint8(zeros(M,N,3));
A(:,:,2)=parrot(:,:,2);
imshow(A); %display G

subplot(1,3,3);
A=uint8(zeros(M,N,3));
A(:,:,3)=parrot(:,:,3);
imshow(A); %display B


