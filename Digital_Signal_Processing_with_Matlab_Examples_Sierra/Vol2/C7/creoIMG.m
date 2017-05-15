% Display and conversion of picture
keatonRGB=imread('gioc.jpg'); %read the image file into a matrix
figure(1)
keaton=keatonRGB(:,:,1); %select one plane
imshow(keaton); pixval on; %display the photo
imwrite(keaton,'c:\aaa\gioc1.jpg','jpg');