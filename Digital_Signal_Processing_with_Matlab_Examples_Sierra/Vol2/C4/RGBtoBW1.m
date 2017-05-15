% Display and conversion of RGB to B&W picture
keatonRGB=imread('wmill1.jpg'); %read the image file into a matrix
figure(1)
keaton=keatonRGB(:,:,1); %select one plane
imshow(keaton); pixval on; %display the photo
imwrite(keaton,'c:\aaa\wmill1.tif','tif');