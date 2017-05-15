% Crop a picture
kei=imread('imagen3.jpg'); %read the image file into a matrix
figure(1)
L=2; T=2;
ckei=kei(L:L+255,T:T+255); %crop
imshow(ckei); pixval on; %display the photo
imwrite(ckei,'c:\aaa\holland1.jpg','jpg');