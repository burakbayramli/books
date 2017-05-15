% Display and conversion of RGB to B&W picture
keatonRGB=imread('Ben.jpg'); %read the image file into a matrix
figure(1)
keaton=keatonRGB(:,:,1); %select one plane
imshow(keaton); pixval on; %display the photo
imwrite(keaton,'c:\aaa\Ben1.tif','tif');