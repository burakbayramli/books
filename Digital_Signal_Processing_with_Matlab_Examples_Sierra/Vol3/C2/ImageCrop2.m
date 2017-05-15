% Crop a picture
kei=imread('face.jpg'); %read the image file into a matrix
figure(1)
L=1; T=1;
ckei=kei(L:L+399,T:T+349); %crop
imshow(ckei); pixval on; %display the photo
imwrite(ckei,'c:\aaa\face1.jpg','jpg');