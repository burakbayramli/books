% Crop a picture
kei=imread('sign.jpg'); %read the image file into a matrix
figure(1)
L=2; T=2;
ckei=kei(L:L+255,T:T+255); %crop
imshow(ckei); pixval on; %display the photo
imwrite(ckei,'c:\aaa\sign1.jpg','jpg');